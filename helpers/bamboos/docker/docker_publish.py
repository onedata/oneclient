#!/usr/bin/env python

# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Copies docker images from private to the official docker repository.

Execute the script with -h flag to learn about script's running options.
"""

import argparse
import json
import re
import sys

from environment import docker

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Copies docker images from private to the official docker '
                'repository.')

parser.add_argument(
    '--repository',
    action='store',
    help='private repository from which pull docker image',
    dest='repository')

parser.add_argument(
    '--user',
    action='store',
    help='username used to login to both the private and official docker '
         'repository',
    dest='user')

parser.add_argument(
    '--password',
    action='store',
    help='password used to login to both the private and official docker '
         'repository',
    dest='password')

parser.add_argument(
    '--organization',
    action='store',
    default='onedata',
    help='organization in official docker repository',
    dest='organization')

parser.add_argument(
    '--name',
    action='store',
    help='name for docker image',
    dest='name')

parser.add_argument(
    '--tag',
    action='store',
    help='tag for docker image',
    dest='tag')


def write_report(priv_image, pub_image):
    """Creates a report consisting of published artifacts (docker images) and
    commands describing how to download those artifacts."""

    with open('docker-publish-report.txt', 'w') as f:
        f.write('Publish report for {0}\n\n'.format(priv_image))
        f.write('Artifact {0}\n'.format(pub_image))
        f.write('\tTo get image run:\n')
        f.write('\t\tdocker pull {0}\n\n'.format(pub_image))


if __name__ == '__main__':
    [args, pass_args] = parser.parse_known_args()

    if len(pass_args) != 1:
        print('Please provide docker build artifact.')
        sys.exit(1)

    with open(pass_args[0], 'r') as f:
        report = json.load(f)
        repository = 'docker.onedata.org'
        name = args.name
        tag = args.tag
        priv_image = report.items()[0][1]

        if not args.name:
            [repository, name, _] = re.split(r'[/:]', priv_image)

        if not args.tag:
            if 'git-tag' in report:
                priv_image = report['git-tag']
                [repository, name, tag] = re.split(r'[/:]', priv_image)
            else:
                print("Missing 'git-tag' in docker build artifact. Aborting...")
                sys.exit(1)

        if args.repository:
            repository = args.repository

        pub_image = '{0}/{1}:{2}'.format(args.organization, name, tag)

        if args.user and args.password:
            docker.login(args.user, args.password, repository)
            docker.login(args.user, args.password)

        docker.pull_image(priv_image)
        docker.tag_image(priv_image, pub_image)
        docker.push_image(pub_image)
        docker.remove_image(priv_image)
        docker.remove_image(pub_image)

        write_report(priv_image, pub_image)
