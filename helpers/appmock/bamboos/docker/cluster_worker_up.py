#!/usr/bin/env python

"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script to brings up a set of cluster nodes. They can form separate
clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json
from environment import common, cluster_worker, dockers_config

parser = common.standard_arg_parser('Bring up bare cluster-worker nodes.')
parser.add_argument(
    '-i-', '--image',
    action='store',
    default=None,
    help='docker image to use for the container',
    dest='image')
parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=None,
    help='path to a directory where the logs will be stored',
    dest='logdir')

args = parser.parse_args()
dockers_config.ensure_image(args, 'image', 'worker')

output = cluster_worker.up(args.image, args.bin, args.dns, args.uid, args.config_path, args.logdir)

print(json.dumps(output))
