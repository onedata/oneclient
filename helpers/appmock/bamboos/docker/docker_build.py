#!/usr/bin/env python

# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs docker build process and publish image to a private docker repository.

Execute the script with -h flag to learn about script's running options.
"""

import argparse
import json
import re
import subprocess

from environment import docker


def cmd(args):
    """Executes shell command and returns result without trailing newline.
    Standard error is redirected to /dev/null."""

    with open('/dev/null', 'w') as dev_null:
        result = subprocess.check_output(args, stderr=dev_null)
    return result.rstrip('\n')


def get_repository_name():
    """Returns repository name."""

    remote = subprocess.check_output(['git', 'remote', '-v'])
    remote = filter(lambda r: r.startswith('origin'), remote.split('\n'))
    return remote[0].split('/')[-1].split('.')[0]


def get_tags():
    """Returns prioritized lists of tags. First tag in the list has the highest
    priority. Each tag is accompanied by its type. Possible types are git-tag,
    git-branch and git-commit."""

    tags = []
    commit = cmd(['git', 'rev-parse', 'HEAD'])
    branch = cmd(['git', 'rev-parse', '--abbrev-ref', 'HEAD'])
    ticket = re.search(r'VFS-\d+', branch)

    try:
        tag = cmd(['git', 'describe', '--contains', '--candidates=0', commit])
        tags.append(('git-tag', tag))
    except subprocess.CalledProcessError:
        pass

    for prefix in ['release/', 'hotfix/']:
        if branch.startswith(prefix):
            tag = '{0}-{1}'.format(branch[len(prefix):], commit[0:7])
            tags.append(('git-branch', tag))

    for prefix in ['feature/', 'bugfix/']:
        if branch.startswith(prefix) and ticket:
            tags.append(('git-branch', ticket.group(0)))

    tags.append(('git-commit', 'ID-{0}'.format(commit[0:10])))

    return tags


def write_short_report(images):
    """Creates a short report consisting of docker images and theirs tag
    types in JSON format."""

    with open('docker-build-list.json', 'w') as f:
        json.dump(dict(images), f, indent=2)


def write_report(name, images, publish):
    """Creates a report consisting of built artifacts (docker images) and
    commands describing how to download those artifacts."""

    with open('docker-build-report.txt', 'w') as f:
        f.write('Build report for {0}\n\n'.format(name))
        f.write('Artifacts:\n\n')
        for _, image in images:
            f.write('Artifact {0}\n'.format(image))
            if publish:
                f.write('\tTo get image run:\n')
                f.write('\t\tdocker pull {0}\n\n'.format(image))


if __name__ == '__main__':
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
        '--repository',
        action='store',
        default='docker.onedata.org',
        help='repository used to publish docker',
        dest='repository')

    try:
        parser.add_argument(
            '--name',
            action='store',
            default=get_repository_name(),
            help='name for docker image',
            dest='name')
    except subprocess.CalledProcessError:
        parser.add_argument(
            '--name',
            action='store',
            required=True,
            help='name for docker image',
            dest='name')

    parser.add_argument(
        '--tag',
        action='append',
        default=[],
        help='custom tag for docker image',
        dest='tags')

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

    [args, pass_args] = parser.parse_known_args()
    tags = get_tags()
    tags.extend([('custom-{0}'.format(i), tag) for i, tag in enumerate(
        args.tags)])

    if args.user and args.password:
        docker.login(args.user, args.password, args.repository)

    image = '{0}/{1}:{2}'.format(args.repository, args.name, tags[0][1])

    docker.build_image(image, pass_args)
    images = [(tags[0][0], image)]

    for tag in tags[1:]:
        image_tag = '{0}/{1}:{2}'.format(args.repository, args.name, tag[1])
        docker.tag_image(image, image_tag)
        images.append((tag[0], image_tag))

    for _, image in images:
        if args.publish:
            docker.push_image(image)

        if args.remove:
            docker.remove_image(image)

    write_short_report(images)
    write_report(args.name, images, args.publish)
