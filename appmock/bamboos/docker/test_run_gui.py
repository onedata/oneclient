#!/usr/bin/env python
# coding=utf-8

"""Author: Michał Ćwiertnia
Copyright (C) 2016-2017 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs gui tests in docker environment.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

import re
import argparse
import os
import platform
import sys
from environment import docker
import glob
import xml.etree.ElementTree as ElementTree
import subprocess
import json
import os.path

script_dir = os.path.dirname(os.path.abspath(__file__))


def add_hosts_arguments():
    parser.add_argument(
        '--onezone-host',
        action='store',
        help='IP address of onezone',
        required=True,
        dest='onezone_host'
    )

    parser.add_argument(
        '--oz-panel-host',
        action='store',
        help='IP address of oz-panel',
        required=True,
        dest='oz_panel_host'
    )

    parser.add_argument(
        '--oneprovider-host',
        action='store',
        help='IP address of oneprovider',
        required=True,
        dest='oneprovider_host'
    )

    parser.add_argument(
        '--op-panel-host',
        action='store',
        help='IP address of op-panel',
        required=True,
        dest='op_panel_host'
    )


def copy_etc_hosts():
    return '''
with open('/etc/hosts', 'a') as f:
    f.write("""
    {etc_hosts_content}
""")
'''.format(etc_hosts_content=get_local_etc_hosts_entries())


def run_docker(command):

    # 128MB or more required for chrome tests to run with xvfb
    run_params = ['--shm-size=128m']

    return docker.run(tty=True,
                      rm=True,
                      interactive=True,
                      name=args.docker_name,
                      workdir=script_dir,
                      reflect=[(script_dir, 'rw'),
                               ('/var/run/docker.sock', 'rw')],
                      volumes=[(os.path.join(os.path.expanduser('~'),
                                             '.docker', 'config.json'),
                               '/root/.docker/config.json', 'ro')],
                      image=args.image,
                      command=['python', '-c', command],
                      run_params=run_params)


def getting_started_local():
    start_env_command = ['python', '-u', 'getting_started_env_up.py',
                         '--scenario', args.scenario, '--zone_name',
                         args.zone_name, '--provider_name', args.provider_name]
    proc = subprocess.Popen(start_env_command, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    output = ''
    for line in iter(proc.stdout.readline, ''):
        print line,
        output = output + line
        sys.stdout.flush()

    split_output = output.split('\n')
    hosts = split_output[len(split_output) - 2]
    hosts_parsed = json.loads(hosts)

    command = ['py.test'] + pass_args + \
              ['--test-type={}'.format(args.test_type),
               args.test_dir,
               '--onezone-host {} {}'.format(args.zone_name, hosts_parsed['onezone_host']),
               '--base-url=https://{}'.format(hosts_parsed['onezone_host']),
               '--oz-panel-host {} {}'.format(args.zone_name, hosts_parsed['oz_panel_host']),
               '--oneprovider-host {} {}'.format(args.provider_name, hosts_parsed['oneprovider_host']),
               '--op-panel-host={} {}'.format(args.provider_name, hosts_parsed['op_panel_host'])]
    subprocess.call(command)


def custom_env():
    additional_code = ''
    command = '''
import os, subprocess, sys, stat

{additional_code}

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

command = ['py.test'] + {args} + ['--test-type={test_type}'] + ['{test_dir}'] + \\
 ['--junitxml={report_path}'] + ['--onezone-host'] + ['{zone_name}'] + ['{onezone_host}'] + \\
 ['--oz-panel-host'] + ['{zone_name}'] + ['{oz_panel_host}'] + ['--oneprovider-host'] + ['{provider_name}'] + ['{oneprovider_host}'] + \\
 ['--op-panel-host'] + ['{provider_name}'] + ['{op_panel_host}']
ret = subprocess.call(command)
sys.exit(ret)
'''

    if args.copy_etc_hosts:
        additional_code = copy_etc_hosts()

    command = command.format(
        args=pass_args,
        uid=os.geteuid(),
        gid=os.getegid(),
        test_dir=args.test_dir,
        shed_privileges=(platform.system() == 'Linux'),
        report_path=args.report_path,
        test_type=args.test_type,
        additional_code=additional_code,
        onezone_host=args.onezone_host,
        zone_name=args.zone_name,
        oz_panel_host=args.oz_panel_host,
        oneprovider_host=args.oneprovider_host,
        provider_name=args.provider_name,
        op_panel_host=args.op_panel_host,
        docker_name=args.docker_name)

    ret = run_docker(command)

    if ret != 0 and not skipped_test_exists(args.report_path):
        ret = 0

    sys.exit(ret)


def getting_started_env():
    additional_code = ''
    command = '''
import os, subprocess, sys, stat, json

{additional_code}

start_env_command = ['python', '-u', 'getting_started_env_up.py', 
'--docker-name', '{docker_name}', '--scenario', '{scenario}', '--zone_name',
'{zone_name}', '--provider_name', '{provider_name}']
proc = subprocess.Popen(start_env_command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

output = ''
for line in iter(proc.stdout.readline, ''):
    print line,
    output = output + line
    sys.stdout.flush()

split_output = output.split('\\n')
hosts = split_output[len(split_output) - 2]
hosts_parsed = json.loads(hosts)

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

command = ['py.test'] + {args} + ['--test-type={test_type}'] + ['{test_dir}'] + \\
 ['--base-url=https://' + str(hosts_parsed['onezone_host'])] + \\
 ['--junitxml={report_path}'] + ['--onezone-host'] + ['{zone_name}'] + [str(hosts_parsed['onezone_host'])] + \\
 ['--oz-panel-host'] + ['{zone_name}'] + [str(hosts_parsed['oz_panel_host'])  + ':9443'] + ['--oneprovider-host'] + ['{provider_name}'] + \\
 [str(hosts_parsed['oneprovider_host'])] + ['--op-panel-host'] + ['{provider_name}'] + \\
 [str(hosts_parsed['op_panel_host']) + ':9443']
ret = subprocess.call(command)
sys.exit(ret)
'''

    if args.copy_etc_hosts:
        additional_code = copy_etc_hosts()

    command = command.format(
        args=pass_args,
        uid=os.geteuid(),
        gid=os.getegid(),
        test_dir=args.test_dir,
        shed_privileges=(platform.system() == 'Linux'),
        report_path=args.report_path,
        test_type=args.test_type,
        docker_name=args.docker_name,
        scenario=args.scenario,
        zone_name=args.zone_name,
        provider_name=args.provider_name,
        additional_code=additional_code)

    ret = run_docker(command)

    if ret != 0 and not skipped_test_exists(args.report_path):
        ret = 0

    sys.exit(ret)


def get_local_etc_hosts_entries():
    """Get entries from local etc/hosts, excluding commented out, blank and
    localhost entries
    Returns a str - content of etc/hosts except excluded lines.
    """

    hosts_content = None
    with open('/etc/hosts', 'r') as f:
        hosts_content = f.read()

    re_exclude_entry = re.compile(r'\s*#.*|.*localhost.*|.*broadcasthost.*|^\s*$')
    entries = filter(lambda line: not re_exclude_entry.match(line),
                     hosts_content.splitlines())

    return '### /etc/hosts from host ###\n' + '\n'.join(entries)


def skipped_test_exists(junit_report_path):
    reports = glob.glob(junit_report_path)
    # if there are many reports, check only the last one
    if reports:
        reports.sort()
        tree = ElementTree.parse(reports[-1])
        testsuite = tree.getroot()
        if testsuite.attrib['skips'] != '0':
            return True
    return False


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Gui Tests.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/gui_builder:latest',
    help='Docker image to use as a test master.',
    dest='image')

parser.add_argument(
    '--test-dir', '-t',
    action='store',
    default='tests/gui',
    help='Test dir to run.',
    dest='test_dir')

parser.add_argument(
    '--report-path', '-r',
    action='store',
    default='test-reports/results.xml',
    help='Path to JUnit tests report',
    dest='report_path')

parser.add_argument(
    '--test-type', '-tt',
    action='store',
    default='gui',
    help='Type of test',
    dest='test_type')

parser.add_argument(
    '--copy-etc-hosts',
    help="Copies local /etc/hosts file to docker (useful when want to test GUI "
         "on locally defined domain)",
    dest='copy_etc_hosts',
    action='store_true'
)

parser.add_argument(
    '--env',
    action='store',
    help='Environment type for tests',
    dest='env',
    required=True
)

parser.add_argument(
    '--docker-name',
    action='store',
    help='Name of docker where tests will be running',
    dest='docker_name',
    default='test_run_gui_docker',
    required=False
)

parser.add_argument(
    '--scenario',
    action='store',
    help='Getting started scenario\'s name',
    dest='scenario',
    default='2_0_oneprovider_onezone',
    required=False
)

parser.add_argument(
    '--zone_name',
    action='store',
    help='Example zone\'s name',
    dest='zone_name',
    default='z1',
    required=False
)


parser.add_argument(
    '--provider_name',
    action='store',
    help='Examples providers\'s name',
    dest='provider_name',
    default='p1',
    required=False
)


[args, pass_args] = parser.parse_known_args()

if args.env == 'custom':
    add_hosts_arguments()
    [args, pass_args] = parser.parse_known_args()
    custom_env()

if args.env == 'getting_started':
    getting_started_env()

if args.env == 'getting_started_local':
    getting_started_local()

if args.env == 'env_up':
    print 'Not implemented yet'
    exit(2)
