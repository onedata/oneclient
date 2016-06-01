#!/usr/bin/env python

"""
Runs integration and acceptance tests in docker environment.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

import argparse
import os
import platform
import sys
from environment import docker
import glob
import xml.etree.ElementTree as ElementTree

script_dir = os.path.dirname(os.path.abspath(__file__))


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
    help="Type of test (cucumber, acceptance, performance, packaging). Default is: acceptance",
    dest='test_type')

parser.add_argument(
    '--runxfail',
    help="Causes test cases marked with xfail to be started normally"
)

[args, pass_args] = parser.parse_known_args()

command = '''
import os, subprocess, sys, stat

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

command = ['py.test'] + {args} + ['--test-type={test_type}'] + ['{test_dir}'] + ['--junitxml={report_path}']
ret = subprocess.call(command)
sys.exit(ret)
'''
command = command.format(
    args=pass_args,
    uid=os.geteuid(),
    gid=os.getegid(),
    test_dir=args.test_dir,
    shed_privileges=(platform.system() == 'Linux'),
    report_path=args.report_path,
    test_type=args.test_type)

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 image=args.image,
                 command=['python', '-c', command])

if ret != 0 and not skipped_test_exists(args.report_path):
    ret = 0

sys.exit(ret)
