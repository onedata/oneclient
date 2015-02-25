#!/usr/bin/env python

"""A script to bring up a DNS server with container (skydns + skydock) that
allow different dockers to see each other by hostnames.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json

from environment import common, dns


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up skydns.')

parser.add_argument(
    '-u', '--uid',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker name',
    dest='uid')

args = parser.parse_args()
output = dns.up(args.uid)
print(json.dumps(output))
