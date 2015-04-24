#!/usr/bin/env python

"""Author: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script to brings up a set of onepanel nodes. They can create separate
clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json

from environment import common, panel


parser = common.standard_arg_parser('Bring up onepanel nodes.')
parser.add_argument(
    'release_path',
    action='store',
    help='path to the release directory of component to be installed')

parser.add_argument(
    '-sp', '--storage_path',
    action='append',
    default=[],
    help='path to the storage used by installed component',
    dest='storage_paths')

args = parser.parse_args()
output = panel.up(args.image, args.bin, args.dns, args.uid, args.config_path,
                  args.release_path, args.storage_paths)

print(json.dumps(output))
