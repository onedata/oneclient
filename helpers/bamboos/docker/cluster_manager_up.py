#!/usr/bin/env python

"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script to bring up a set of oneprovider cm nodes. They can create separate
clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json

from environment import common, cluster_manager


parser = common.standard_arg_parser('Bring up cluster_manager nodes.')
parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=None,
    help='path to a directory where the logs will be stored',
    dest='logdir')

args = parser.parse_args()
output = cluster_manager.up(args.image, args.bin, args.dns, args.uid,
                     args.config_path, args.logdir)

print(json.dumps(output))
