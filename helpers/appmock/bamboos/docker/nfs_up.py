#!/usr/bin/env python
# coding=utf-8

"""Authors: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a nfs server.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import json

from environment import nfs
from environment import common

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up nfs server.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default='onedata/worker:v57',
    help='docker image to use for the container',
    dest='image')

args = parser.parse_args()
config = nfs.up(args.image, common.generate_uid(), 'storage')

print(json.dumps(config))
