#!/usr/bin/env python
# coding=utf-8

"""Authors: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a S3 storage.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import json

from environment import s3, common

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up S3 storage.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default='lphoward/fake-s3',
    help='docker image to use for the container',
    dest='image')

parser.add_argument(
    '-b', '--bucket',
    action='append',
    default=[],
    help='bucket name',
    dest='buckets')

parser.add_argument(
    '-u', '--uid',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker names',
    dest='uid')

args = parser.parse_args()
config = s3.up(args.image, args.buckets, 'storage', args.uid)

print(json.dumps(config))
