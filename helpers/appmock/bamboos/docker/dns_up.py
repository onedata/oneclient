#!/usr/bin/env python
# coding=utf-8

"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script to bring up a DNS server with container (skydns + skydock) that
allow different dockers to see each other by hostnames.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json
import sys

from environment import common, dns, docker


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up skydns.')

parser.add_argument(
    '-u', '--uid',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker name',
    dest='uid')

parser.add_argument(
    '--root-servers',
    action='store',
    default=[],
    help='list of root servers for non-authoritative domain delegation, in format domain1/ip,domain2/ip...',
    dest='root_servers')

parser.add_argument(
    '-r', '--restart',
    action='store',
    default='none',
    help='use this flag with a docker name, then a dns server with this name will be restarted (uid arg will be ignored)',
    dest='restart')

args = parser.parse_args()
if args.restart is not 'none':
    try:
        docker.inspect(args.restart)
    except:
        sys.exit(1)

if args.root_servers:
    args.root_servers = args.root_servers.split(',')
output = dns.up(args.uid, args.root_servers, args.restart)
print(json.dumps(output))
