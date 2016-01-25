#!/usr/bin/env python

# coding=utf-8
"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs a command in a dockerized development environment. The files are copied
from 'source directory' to 'output directory' and then the command is ran.
The copy operation is optimized, so that only new and changed files are copied.
The script uses user's SSH keys in case dependency fetching is needed.

Unknown arguments will be passed to the command.

Run the script with -h flag to learn about script's running options.
"""

from os.path import expanduser
import argparse
import os
import platform
import sys

from environment import docker


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run a command inside a dockerized development environment.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default='onedata/builder',
    help='docker image to use for building',
    dest='image')

parser.add_argument(
    '-s', '--src',
    action='store',
    default=os.getcwd(),
    help='source directory to run make from',
    dest='src')

parser.add_argument(
    '-d', '--dst',
    action='store',
    default=None,
    help='destination directory where the build will be stored; defaults '
         'to source dir if unset',
    dest='dst')

parser.add_argument(
    '-k', '--keys',
    action='store',
    default=expanduser("~/.ssh"),
    help='directory of ssh keys used for dependency fetching',
    dest='keys')

parser.add_argument(
    '-r', '--reflect-volume',
    action='append',
    default=[],
    help="host's paths to reflect in container's filesystem",
    dest='reflect')

parser.add_argument(
    '-c', '--command',
    action='store',
    default='make',
    help='command to run in the container',
    dest='command')

parser.add_argument(
    '-w', '--workdir',
    action='store',
    default=None,
    help='path to the working directory; defaults to destination dir if unset',
    dest='workdir')

parser.add_argument(
    '-e', '--env',
    action='append',
    default=[],
    help='env variables to set in the environment',
    dest='envs')

parser.add_argument(
    '--group',
    action='append',
    default=[],
    help='system groups user should be a part of',
    dest='groups')

parser.add_argument(
    '--privileged',
    action='store_true',
    default=False,
    help='run the container with --privileged=true',
    dest='privileged')

[args, pass_args] = parser.parse_known_args()

destination = args.dst if args.dst else args.src
workdir = args.workdir if args.workdir else destination

command = '''
import os, shutil, subprocess, sys

os.environ['HOME'] = '/root'

ssh_home = '/root/.ssh'
if {shed_privileges}:
    useradd = ['useradd', '--create-home', '--uid', '{uid}', 'maketmp']
    if {groups}:
        useradd.extend(['-G', ','.join({groups})])

    subprocess.call(useradd)

    os.environ['PATH'] = os.environ['PATH'].replace('sbin', 'bin')
    os.environ['HOME'] = '/home/maketmp'
    ssh_home = '/home/maketmp/.ssh'
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

if '{src}' != '{dst}':
    ret = subprocess.call(['rsync', '--archive', '/tmp/src/', '{dst}'])
    if ret != 0:
        sys.exit(ret)

shutil.copytree('/tmp/keys', ssh_home)
for root, dirs, files in os.walk(ssh_home):
    for dir in dirs:
        os.chmod(os.path.join(root, dir), 0o700)
    for file in files:
        os.chmod(os.path.join(root, file), 0o600)

sh_command = 'eval $(ssh-agent) > /dev/null; ssh-add 2>&1; {command} {params}'
ret = subprocess.call(['sh', '-c', sh_command])
sys.exit(ret)
'''
command = command.format(
    command=args.command,
    params=' '.join(pass_args),
    uid=os.geteuid(),
    gid=os.getegid(),
    src=args.src,
    dst=destination,
    shed_privileges=(platform.system() == 'Linux' and os.geteuid() != 0),
    groups=args.groups)

reflect = [(destination, 'rw')]
reflect.extend(zip(args.reflect, ['rw'] * len(args.reflect)))

split_envs = [e.split('=') for e in args.envs]
envs = {kv[0]: kv[1] for kv in split_envs}

ret = docker.run(tty=True,
                 interactive=True,
                 rm=True,
                 reflect=reflect,
                 volumes=[(args.keys, '/tmp/keys', 'ro'),
                          (args.src, '/tmp/src', 'ro')],
                 envs=envs,
                 workdir=workdir,
                 image=args.image,
                 run_params=(['--privileged=true'] if args.privileged else []),
                 command=['python', '-c', command])
sys.exit(ret)
