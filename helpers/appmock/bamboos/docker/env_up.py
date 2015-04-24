#!/usr/bin/env python
# coding=utf-8

"""Author: Łukasz Opioła
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json
import os

from environment import env


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up onedata environment.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default=env.default('image'),
    help='the image to use for the components',
    dest='image')

parser.add_argument(
    '-bp', '--bin-provider',
    action='store',
    default=env.default('bin_op'),
    help='the path to oneprovider repository (precompiled)',
    dest='bin_op')

parser.add_argument(
    '-bg', '--bin-gr',
    action='store',
    default=env.default('bin_gr'),
    help='the path to globalregistry repository (precompiled)',
    dest='bin_gr')

parser.add_argument(
    '-ba', '--bin-appmock',
    action='store',
    default=env.default('bin_am'),
    help='the path to appmock repository (precompiled)',
    dest='bin_am')

parser.add_argument(
    '-bc', '--bin-client',
    action='store',
    default=env.default('bin_oc'),
    help='the path to oneclient repository (precompiled)',
    dest='bin_oc')

parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=env.default('logdir'),
    help='path to a directory where the logs will be stored',
    dest='logdir')

parser.add_argument(
    'config_path',
    action='store',
    help='path to json configuration file')

args = parser.parse_args()

output = env.up(args.image, args.bin_am, args.bin_gr, args.bin_op, args.bin_oc,
                args.logdir, args.config_path)

print(json.dumps(output))
