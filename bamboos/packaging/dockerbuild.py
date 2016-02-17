#!/usr/bin/env python

# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs docker build process and publish results to a docker registry.

Execute the script with -h flag to learn about script's running options.
"""

import argparse
import subprocess

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run docker build process and publish results to registry.')

parser.add_argument(
    '--user',
    action='store',
    help='username used to login to the docker repository',
    dest='user')

parser.add_argument(
    '--password',
    action='store',
    help='password used to login to the docker repository',
    dest='password')

parser.add_argument(
    '--email',
    action='store',
    help='email used to login to the docker repository',
    dest='email')

parser.add_argument(
    '--repository',
    action='store',
    default='docker.onedata.org',
    help='repository used to publish docker',
    dest='repository')

try:
    remote = subprocess.check_output(['git', 'remote', '-v'])
    remote = filter(lambda r: r.startswith('origin'), remote.split('\n'))
    name = remote[0].split('/')[-1].split('.')[0]
    parser.add_argument(
        '--name',
        action='store',
        default=name,
        help='name for docker image',
        dest='name')
except subprocess.CalledProcessError:
    parser.add_argument(
        '--name',
        action='store',
        required=True,
        help='name for docker image',
        dest='name')

try:
    tag = subprocess.check_output(['git', 'describe', '--tags', '--always'])
    tag = tag.strip().replace('-', '.')
    parser.add_argument(
        '--tag',
        action='store',
        default=tag,
        help='tag for docker image',
        dest='tag')
except subprocess.CalledProcessError:
    parser.add_argument(
        '--tag',
        action='store',
        required=True,
        help='tag for docker image',
        dest='tag')

parser.add_argument(
    '--publish',
    action='store_true',
    default=False,
    help='publish docker to the repository',
    dest='publish')

parser.add_argument(
    '--remove',
    action='store_true',
    default=False,
    help='remove local docker image after build',
    dest='remove')

if __name__ == '__main__':
    [args, pass_args] = parser.parse_known_args()

    if args.user and args.password and args.email:
        subprocess.check_call(['docker', 'login', '-u', args.user, '-p',
                               args.password, '-e', args.email,
                               args.repository])

    image = '{0}/{1}:{2}'.format(args.repository, args.name, args.tag)
    subprocess.check_call(['docker', 'build', '--force-rm', '-t', image] +
                          pass_args)

    if args.publish:
        subprocess.check_call(['docker', 'push', image])

    if args.remove:
        subprocess.check_call(['docker', 'rmi', '-f', image])
