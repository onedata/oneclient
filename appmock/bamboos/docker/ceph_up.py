#!/usr/bin/env python
# coding=utf-8

"""Authors: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a Ceph storage cluster.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json

from environment import ceph, common

parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='Bring up Ceph storage cluster.')

parser.add_argument(
        '-i', '--image',
        action='store',
        default='docker.onedata.org/ceph-base',
        help='docker image to use for the container',
        dest='image')

parser.add_argument(
        '-p', '--pool',
        action='append',
        default=[],
        help='pool name and number of placement groups in format name:pg_num',
        dest='pools')

parser.add_argument(
        '-u', '--uid',
        action='store',
        default=common.generate_uid(),
        help='uid that will be concatenated to docker names',
        dest='uid')

args = parser.parse_args()
pools = map(lambda pool: tuple(pool.split(':')), args.pools)

config = ceph.up(args.image, pools, 'storage', args.uid)

print(json.dumps(config))
