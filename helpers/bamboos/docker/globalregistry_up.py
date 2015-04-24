#!/usr/bin/env python

"""A script to bring up a set of Global Registry nodes along with databases.
They can create separate clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json

from environment import common, globalregistry


parser = common.standard_arg_parser('Bring up globalregistry nodes.')
parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=None,
    help='path to a directory where the logs will be stored',
    dest='logdir')

args = parser.parse_args()
output = globalregistry.up(args.image, args.bin, args.logdir, args.dns, args.uid,
                           args.config_path)
print(json.dumps(output))
