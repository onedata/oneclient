#!/usr/bin/env python3

"""Runs integration tests."""

import argparse
import os
import platform
import sys
import subprocess
import time
import json

script_dir = os.path.dirname(os.path.realpath(__file__))
helpers_dir = os.path.join(script_dir, 'helpers')
docker_dir = os.path.join(helpers_dir, 'bamboos', 'docker')
sys.path.insert(0, docker_dir)
from environment import docker, dockers_config

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

parser.add_argument(
    '--gdb',
    action='store_true',
    default=False,
    help='run tests in GDB')

parser.add_argument(
    '--onenv-config',
    action='store',
    help='run tests in one-env environment',
    dest='onenv_config')

parser.add_argument(
    '--no-clean',
    action='store_true',
    default=False,
    help='do not clean the one-env deployment after test',
    dest='no_clean')

parser.add_argument(
    '--image', '-i',
    action='store',
    default=None,
    help='docker image to use as a test master',
    dest='image')

parser.add_argument(
    '--release',
    action='store',
    default='release',
    help='release directory to run tests from',
    dest='release')

parser.add_argument(
    '--suite',
    action='append',
    default=[],
    help='name of the test suite',
    dest='suites')

parser.add_argument(
    '--no-shed-privileges',
    action='store_true',
    default=False,
    help='Run tests as root in container',
    dest='no_shed_privileges')




[args, pass_args] = parser.parse_known_args()
dockers_config.ensure_image(args, 'image', 'builder')


script_dir = os.path.dirname(os.path.realpath(__file__))
if args.onenv_config is not None:
    base_test_dir = os.path.join(os.path.realpath(args.release), 'test',
                                'onenv_tests')
else:
    base_test_dir = os.path.join(os.path.realpath(args.release), 'test',
                                'integration')

test_dirs = map(lambda suite: os.path.join(base_test_dir, suite), args.suites)
if not test_dirs:
    test_dirs = [base_test_dir]

envs={'BASE_TEST_DIR': base_test_dir,
      'PYTHONWARNINGS': 'ignore:Unverified HTTPS request',
      'BACKWARD_CXX_SOURCE_PREFIXES': os.path.join(script_dir, args.release)}

# Setup oneenv environment
if args.onenv_config is not None:
    if not os.path.exists(args.onenv_config):
        print(f'Error: No such one-env file {args.onenv_config}')
        sys.exit(1)

    try:
        up_output = subprocess.check_output(['./one-env/onenv', 'up', args.onenv_config])
    except subprocess.CalledProcessError as e:
        print(f'Failed to start onenv up due to: {e.output}')
        sys.exit(1)

    environment_ready = False
    retries = 3
    while (not environment_ready) and retries > 0:
        try:
            subprocess.check_call(['./one-env/onenv', 'wait'])
            environment_ready = True
        except subprocess.CalledProcessError as e:
            retries =- 1
            time.sleep(5)
            print(f'Waiting for one-env environment setup...')

    if retries == 0:
        print(f'Failed to start K8S environment from {args.onenv_config}')
        sys.exit(1)

    print(f'One-env environment ready')

    # Get Onezone IP
    get_onezoneip_cli = "kubectl get pod dev-onezone-0 --template {{.status.podIP}}"
    onezone_ip = subprocess.check_output(get_onezoneip_cli.split(' ')).strip()
    envs['ONEZONE_IP'] = onezone_ip.decode('utf-8')

    # Get Oneprovider IP
    get_oneproviderip_cli = "kubectl get pod dev-oneprovider-krakow-0 --template {{.status.podIP}}"
    oneprovider_ip = subprocess.check_output(get_oneproviderip_cli.split(' ')).strip()
    envs['ONEPROVIDER_IP'] = oneprovider_ip.decode('utf-8')

    get_oneprovider2ip_cli = "kubectl get pod dev-oneprovider-paris-0 --template {{.status.podIP}}"
    oneprovider_2_ip = subprocess.check_output(get_oneprovider2ip_cli.split(' ')).strip()
    envs['ONEPROVIDER_2_IP'] = oneprovider_2_ip.decode('utf-8')

    # Get Ceph storage IP
    get_endpoints_cli = f'kubectl get endpoints -lcomponent=volume-ceph -o json'
    endpoints = subprocess.check_output(get_endpoints_cli.split(' ')).strip()
    for item in json.loads(endpoints)['items']:
        if item['metadata']['name'] == 'dev-volume-ceph-krakow':
            envs['CEPH_MONITOR_IP'] = item['subsets'][0]['addresses'][0]['ip']
            break

    if 'CEPH_MONITOR_IP' not in envs:
        print(f'Error: Cannot find ceph storage volume pod in:\n {endpoints}')
        sys.exit(1)

    get_endpoints_cli = f'kubectl get endpoints -lcomponent=volume-s3 -o json'
    endpoints = subprocess.check_output(get_endpoints_cli.split(' ')).strip()
    for item in json.loads(endpoints)['items']:
        if item['metadata']['name'] == 'dev-volume-s3-krakow':
            envs['S3_SERVER_IP'] = item['subsets'][0]['addresses'][0]['ip']
            break

    if 'S3_SERVER_IP' not in envs:
        print(f'Error: Cannot find s3 storage volume pod in:\n {endpoints}')
        sys.exit(1)

    print(f'Environment passed to pytest container: {str(envs)}')


command = '''
import os, subprocess, sys, stat, shutil

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

if {gdb}:
    command = ['gdb', 'python3', '-silent', '-statistics', '-ex', """run -c "
import pytest
pytest.main({args} + ['{test_dirs}'])" """]
else:
    command = ['python3'] + ['-m'] + ['pytest'] + {args} + ['{test_dirs}']

if(not os.path.exists('{script_dir}/{release}/test/integration/onedatafs_test/onedatafs.so')):
    shutil.copyfile('{script_dir}/{release}/onedatafs_py3.so',
                    '{script_dir}/{release}/test/integration/onedatafs_test/onedatafs.so')

ret = subprocess.call(command)
sys.exit(ret)
'''
command = command.format(
    args=pass_args,
    uid=os.geteuid(),
    gid=os.getegid(),
    test_dirs="', '".join(test_dirs),
    shed_privileges=(platform.system() == 'Linux') and not args.no_shed_privileges,
    gdb=args.gdb,
    script_dir=script_dir,
    release=args.release)

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 image=args.image,
                 envs=envs,
                 run_params=['--privileged'] if args.gdb or args.no_shed_privileges else [],
                 command=['python', '-c', command])

if not args.no_clean:
    try:
        up_output = subprocess.check_output(['./one-env/onenv', 'clean'])
    except subprocess.CalledProcessError as e:
        print(f'Failed to clean onenv due to: {e.output}')
        sys.exit(1)

sys.exit(ret)
