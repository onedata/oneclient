#!/usr/bin/env python

"""A script that prepares a set dockers with oneclient instances that are
configured and ready to start.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json

from environment import client, common


parser = common.standard_arg_parser(
    'Set up dockers with oneclient preconfigured.')

args = parser.parse_args()
output = client.up(args.image, args.bindir, args.dns, args.uid,
                   args.config_path)

print(json.dumps(output))
