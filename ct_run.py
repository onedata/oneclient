#!/usr/bin/env python

"""Runs integration tests."""

import argparse
import os
import platform
import sys
import re

script_dir = os.path.dirname(os.path.realpath(__file__))
docker_dir = os.path.join(script_dir, 'bamboos', 'docker')
sys.path.insert(0, docker_dir)
from environment import docker

def parse_valgrind_log_error_count(log_file):
    """
    Parses valgrind memcheck file and returns the identified error count.
    """
    with open(log_file, 'r') as f:
        regex = re.compile("ERROR SUMMARY:\s(\d+)\serrors")
        for line in f:
            match = re.search(regex, line)
            if match:
                return int(match.groups()[0])
        raise SystemExit("Invalid Valgrind memcheck report file: "+log_file)


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

parser.add_argument(
    '--gdb',
    action='store_true',
    default=False,
    help='run tests in GDB')

parser.add_argument(
    '--valgrind',
    action='store_true',
    default=False,
    help='run tests under Valgrind')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/builder',
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

[args, pass_args] = parser.parse_known_args()
script_dir = os.path.dirname(os.path.realpath(__file__))
base_test_dir = os.path.join(os.path.realpath(args.release), 'test',
                             'integration')
test_dirs = map(lambda suite: os.path.join(base_test_dir, suite), args.suites)
if args.valgrind:
    if len(test_dirs) != 1: 
        raise SystemExit('Valgrind test run requires specification of a single '
                         'test case suite, e.g. \'--suite ceph_helper_test\'')
    if args.gdb: 
        raise SystemExit('GDB and Valgrind cannot be used simultanously for '
                         'tests')

if not test_dirs:
    test_dirs = [base_test_dir]

command = '''
import os, subprocess, sys, stat

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

if {gdb}:
    command = ['gdb', 'python', '-silent', '-ex', """run -c "
import pytest
pytest.main({args} + ['{test_dirs}'])" """]
elif {valgrind}:
    command = ['valgrind'] \\
            + ['--gen-suppressions=all'] \\
            + ['--suppressions=valgrind.supp'] \\
            + ['--track-origins=yes'] \\
            + ['--log-file=valgrind-{suite}.txt'] \\
            + ['--show-leak-kinds=definite'] \\
            + ['--leak-check=full'] \\
            + ['py.test'] + {args} + ['{test_dirs}']
else:
    command = ['py.test'] + {args} + ['{test_dirs}']

ret = subprocess.call(command)
sys.exit(ret)
'''
command = command.format(
    args=pass_args,
    uid=os.geteuid(),
    gid=os.getegid(),
    test_dirs="', '".join(test_dirs),
    shed_privileges=(platform.system() == 'Linux'),
    gdb=args.gdb,
    valgrind=args.valgrind,
    suite=(args.suites[0] if args.valgrind else "', '".join(test_dirs)))

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 image=args.image,
                 envs={'BASE_TEST_DIR': base_test_dir},
                 run_params=['--privileged'] if (args.gdb or args.valgrind) else [],
                 command=['python', '-c', command])

# If test suite succeeded and Valgrind was enabled, parse the memcheck report
# and return error if any errors were identified
if ret == 0 and args.valgrind:
    ret = parse_valgrind_log_error_count("valgrind-"+args.suites[0]+".txt")

sys.exit(ret)
