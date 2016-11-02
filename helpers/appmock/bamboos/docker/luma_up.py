#!/usr/bin/env python
# coding=utf-8

"""Authors: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a python LUMA.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json
import os

from environment import luma, common

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up LUMA python.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default='onedata/worker',
    help='docker image to use for the container',
    dest='image')

parser.add_argument(
    '-b', '--bin',
    action='store',
    default=os.getcwd(),
    help='path to the code repository (precompiled)',
    dest='bin')

parser.add_argument(
    'config_path',
    action='store',
    default=None,
    nargs='?',
    help='path to json configuration file')

parser.add_argument(
    '-u', '--uid',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker names',
    dest='uid')

args = parser.parse_args()

env_config = None
if args.config_path:
    common.parse_json_config_file(args.config_path)

config = luma.up(args.image, args.bin, env_config, args.uid)

print(json.dumps(config))
