#!/usr/bin/env python

"""Runs integration tests."""

import argparse
import os
import platform
import sys

script_dir = os.path.dirname(os.path.realpath(__file__))
docker_dir = os.path.join(script_dir, 'bamboos', 'docker')
sys.path.insert(0, docker_dir)
from environment import docker

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

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
    '--performance', '-p',
    action='store_true',
    default=False,
    help='run performance tests',
    dest='performance')

parser.add_argument(
    '--suite', '-s',
    action='append',
    default=[],
    help='name of the test suite',
    dest='suites')

[args, pass_args] = parser.parse_known_args()
script_dir = os.path.dirname(os.path.realpath(__file__))
base_test_dir = os.path.join(os.path.realpath(args.release), 'test',
                             'integration')
test_dirs = map(lambda suite: os.path.join(base_test_dir, suite), args.suites)
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

command = ['py.test'] + {args} + ['{test_dirs}']
ret = subprocess.call(command)
sys.exit(ret)
'''
command = command.format(
    args=pass_args,
    uid=os.geteuid(),
    gid=os.getegid(),
    test_dirs="', '".join(test_dirs),
    shed_privileges=(platform.system() == 'Linux'))

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 image=args.image,
                 envs={'PERFORMANCE': args.performance,
                       'BASE_TEST_DIR': base_test_dir},
                 command=['python', '-c', command])
sys.exit(ret)
