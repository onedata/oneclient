#!/usr/bin/env python

"""
Runs integration and acceptance tests in docker environment.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

import re
import argparse
import os
import platform
import sys
import time

from environment.common import HOST_STORAGE_PATH
from environment import docker, remove_dockers_and_volumes
import glob
import xml.etree.ElementTree as ElementTree

script_dir = os.path.dirname(os.path.abspath(__file__))


def get_local_etc_hosts_entries():
    """Get entries from local etc/hosts, excluding commented out, blank and localhost entries
    Returns a str - content of etc/hosts except excluded lines.
    """

    hosts_content = None
    with open('/etc/hosts', 'r') as f:
        hosts_content = f.read()

    re_exclude_entry = re.compile(r'\s*#.*|.*localhost.*|.*broadcasthost.*|^\s*$')
    entries = filter(lambda line: not re_exclude_entry.match(line), hosts_content.splitlines())

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
    description='Run Common Tests.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='Docker image to use as a test master.',
    dest='image')

parser.add_argument(
    '--test-dir', '-t',
    action='store',
    default='tests/acceptance',
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
    default="acceptance",
    help="Type of test (acceptance, env_up, performance, packaging, gui)",
    dest='test_type')


parser.add_argument(
    '--copy-etc-hosts',
    help="Copies local /etc/hosts file to docker (useful when want to test GUI on locally defined domain)",
    dest='copy_etc_hosts',
    action='store_true'
)

parser.add_argument(
    '--env-file', '-e',
    action='store',
    default=None,
    help="path to description of test environment in .json file",
    dest='env_file')

parser.add_argument(
    '--docker-name',
    action='store',
    help="Name of docker container where tests will be running",
    dest='docker_name',
    default='test_run_docker_{}'.format(int(time.time()))
)    

[args, pass_args] = parser.parse_known_args()

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

command = ['py.test'] + ['--test-type={test_type}'] + ['{test_dir}'] + ['--docker-name', '{docker_name}'] + {args} + {env_file} + ['--junitxml={report_path}']
ret = subprocess.call(command)
sys.exit(ret)
'''

additional_code = ''

if args.copy_etc_hosts:
    additional_code = '''
with open('/etc/hosts', 'a') as f:
    f.write("""
    {etc_hosts_content}
""")
'''.format(etc_hosts_content=get_local_etc_hosts_entries())

if '--getting-started' in pass_args:
    additional_code += """
with open('/etc/sudoers.d/all', 'w+') as file:
    file.write("\\nALL       ALL = (ALL) NOPASSWD: ALL\\n")
"""

command = command.format(
    args=pass_args,
    uid=os.geteuid(),
    gid=os.getegid(),
    test_dir=args.test_dir,
    docker_name=args.docker_name,
    shed_privileges=(platform.system() == 'Linux'),
    report_path=args.report_path,
    test_type=args.test_type,
    additional_code=additional_code,
    env_file=["--env-file={}".format(args.env_file)] if args.env_file else "[]"
)

# 128MB or more required for chrome tests to run with xvfb
run_params = ['--shm-size=128m']

remove_dockers_and_volumes()

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 name=args.docker_name,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw'),
                          ('/etc/passwd', 'ro'),
                          (HOST_STORAGE_PATH, 'rw')],
                 volumes=[(os.path.join(os.path.expanduser('~'),
                                        '.docker'), '/tmp/.docker', 'rw')],
                 image=args.image,
                 command=['python', '-c', command],
                 run_params=run_params)

if ret != 0 and not skipped_test_exists(args.report_path):
    ret = 0

sys.exit(ret)
