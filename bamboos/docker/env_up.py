#!/usr/bin/env python

"""Brings up a DNS server with container (skydns + skydock) that allow
different dockers to see each other by hostnames.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json
import os

from environment import appmock, client, common, globalregistry, provider


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up onedata environment.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default='onedata/worker',
    help='the image to use for the components',
    dest='image')

parser.add_argument(
    '-bp', '--bin-provider',
    action='store',
    default=os.getcwd() + '/oneprovider',
    help='the path to oneprovider repository (precompiled)',
    dest='bin_op')

parser.add_argument(
    '-bg', '--bin-gr',
    action='store',
    default=os.getcwd() + '/globalregistry',
    help='the path to globalregistry repository (precompiled)',
    dest='bin_gr')

parser.add_argument(
    '-ba', '--bin-appmock',
    action='store',
    default=os.getcwd() + '/appmock',
    help='the path to appmock repository (precompiled)',
    dest='bin_am')

parser.add_argument(
    '-bc', '--bin-client',
    action='store',
    default=os.getcwd() + '/oneclient',
    help='the path to oneclient repository (precompiled)',
    dest='bin_oc')

parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=None,
    help='path to a directory where the logs will be stored',
    dest='logdir')

parser.add_argument(
    'config_path',
    action='store',
    help='path to json configuration file')

args = parser.parse_args()
config = common.parse_json_file(args.config_path)
uid = common.generate_uid()

output = {
    'docker_ids': [],
    'gr_nodes': [],
    'gr_db_nodes': [],
    'op_ccm_nodes': [],
    'op_worker_nodes': [],
    'appmock_nodes': [],
    'client_nodes': []
}

# Start DNS
[dns], dns_output = common.set_up_dns('auto', uid)
common.merge(output, dns_output)

# Start appmock instances
if 'appmock' in config:
    am_output = appmock.up(args.image, args.bin_am, dns, uid, args.config_path)
    common.merge(output, am_output)

# Start globalregistry instances
if 'globalregistry' in config:
    gr_output = globalregistry.up(args.image, args.bin_gr, args.logdir, dns, uid,
                                  args.config_path)
    common.merge(output, gr_output)

# Start oneprovider_node instances
if 'oneprovider_node' in config:
    op_output = provider.up(args.image, args.bin_op, args.logdir, dns, uid,
                            args.config_path)
    common.merge(output, op_output)

# Start oneclient instances
if 'oneclient' in config:
    oc_output = client.up(args.image, args.bin_oc, dns, uid, args.config_path)
    common.merge(output, oc_output)

print(json.dumps(output))
