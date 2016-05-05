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
import socket

from environment import common, panel

parser = common.standard_arg_parser('Bring up onepanel nodes.')

parser.add_argument(
    '--oz-ip',
    action='store',
    default=socket.gethostbyname('onedata.org'),
    help='onezone IP address',
    dest='oz_ip')

args = parser.parse_args()
output = panel.up(args.image, args.bin, args.dns, args.uid, args.config_path,
                  args.oz_ip)

print(json.dumps(output))
