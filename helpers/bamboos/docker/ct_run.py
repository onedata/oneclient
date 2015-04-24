#!/usr/bin/env python

"""
Runs oneprovider integration tests, providing Erlang's ct_run with every
environmental argument it needs for successful run. The output is put into
'test_distributed/logs'. The (init|end)_per_suite "testcases" are removed from
the surefire.xml output.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

import argparse
import glob
import os
import platform
import sys
import time

sys.path.insert(0, 'bamboos/docker')
from environment import docker

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='docker image to use as a test master',
    dest='image')

parser.add_argument(
    '--suite', '-s',
    action='append',
    help='name of the test suite',
    dest='suites')

parser.add_argument(
    '--case', '-c',
    action='append',
    help='name of the test case',
    dest='cases')

parser.add_argument(
    '--perf', '-p',
    action='store_true',
    default=False,
    help='run performance tests',
    dest='perf')

args = parser.parse_args()
script_dir = os.path.dirname(os.path.abspath(__file__))
uid = str(int(time.time()))

ct_command = ['ct_run',
              '-no_auto_compile',
              '-dir', '.',
              '-logdir', './logs/',
              '-ct_hooks', 'cth_surefire', '[{path, "surefire.xml"}]',
              '-noshell',
              '-name', 'testmaster@testmaster.{0}.dev.docker'.format(uid),
              '-include', '../include', '../deps']

code_paths = ['-pa', os.path.join(script_dir, 'ebin')]
code_paths.extend(glob.glob(os.path.join(script_dir, 'deps', '*', 'ebin')))
ct_command.extend(code_paths)

if args.suites:
    ct_command.append('-suite')
    ct_command.extend(args.suites)

if args.cases:
    ct_command.append('-case')
    ct_command.extend(args.cases)

if args.perf:
    ct_command.extend(['-env', 'perf_test', 'true'])

command = '''
import os, subprocess, sys, stat

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

command = {cmd}
ret = subprocess.call(command)

import xml.etree.ElementTree as ElementTree, glob, re
for file in glob.glob('logs/*/surefire.xml'):
    tree = ElementTree.parse(file)
    for suite in tree.findall('.//testsuite'):
        for test in suite.findall('testcase'):
            match = re.match('(init|end)_per_suite', test.attrib['name'])
            if match is not None:
                suite.remove(test)
    tree.write(file)

sys.exit(ret)
'''
command = command.format(
    uid=os.geteuid(),
    gid=os.getegid(),
    cmd=ct_command,
    shed_privileges=(platform.system() == 'Linux'))

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=os.path.join(script_dir, 'test_distributed'),
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 name='testmaster_{0}'.format(uid),
                 hostname='testmaster.{0}.dev.docker'.format(uid),
                 image=args.image,
                 command=['python', '-c', command])
sys.exit(ret)
