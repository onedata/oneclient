#!/usr/bin/env python

"""
Brings up a DNS server with container (skydns + skydock) that allow
different dockers to see each other by hostnames.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import common
import json
import os


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up onedata environment.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='the image to use for the components',
    dest='image')

parser.add_argument(
    '--bin-provider', '-bp',
    action='store',
    default=os.getcwd() + '/oneprovider',
    help='the path to oneprovider repository (precompiled)',
    dest='bin_op')

parser.add_argument(
    '--bin-gr', '-bg',
    action='store',
    default=os.getcwd() + '/globalregistry',
    help='the path to globalregistry repository (precompiled)',
    dest='bin_gr')

parser.add_argument(
    '--bin-appmock', '-ba',
    action='store',
    default=os.getcwd() + '/appmock',
    help='the path to appmock repository (precompiled)',
    dest='bin_am')

parser.add_argument(
    '--bin-client', '-bc',
    action='store',
    default=os.getcwd() + '/oneclient',
    help='the path to oneclient repository (precompiled)',
    dest='bin_oc')

parser.add_argument(
    'config_path',
    action='store',
    help='path to gen_dev_args.json file that will be used to configure the environment')

args = parser.parse_args()
config = common.parse_json_file(args.config_path)
uid = common.generate_uid()

# Start DNS
dns_output = common.run_script_return_dict('dns_up.py', ['--uid', uid])
dns = dns_output['dns']

# Start globalregistry instances
gr_output = {
    'docker_ids': [],
    'gr_nodes': [],
    'gr_db_nodes': []
}
if 'globalregistry' in config:
    gr_output = common.run_script_return_dict('globalregistry_up.py', [
        '--image', args.image,
        '--bin', args.bin_gr,
        '--dns', dns,
        '--uid', uid,
        args.config_path])

# Start oneprovider_node instances
op_output = {
    'docker_ids': [],
    'op_ccm_nodes': [],
    'op_worker_nodes': []
}
if 'oneprovider_node' in config:
    op_output = common.run_script_return_dict('provider_up.py', [
        '--image', args.image,
        '--bin', args.bin_op,
        '--dns', dns,
        '--uid', uid,
        args.config_path])

# Start appmock instances
am_output = {
    'docker_ids': [],
    'appmock_nodes': []
}
if 'appmock' in config:
    am_output = common.run_script_return_dict('appmock_up.py', [
        '--image', args.image,
        '--bin', args.bin_am,
        '--dns', dns,
        '--uid', uid,
        args.config_path])

# Start oneclient instances
oc_output = {
    'docker_ids': [],
    'client_nodes': []
}
if 'oneclient' in config:
    oc_output = common.run_script_return_dict('client_up.py', [
        '--image', args.image,
        '--bin', args.bin_oc,
        '--dns', dns,
        '--uid', uid,
        args.config_path])

# Gather output from all components' starting and print it
output = {
    'dns': dns_output['dns'],
    'docker_ids': dns_output['docker_ids'] + gr_output['docker_ids'] + op_output['docker_ids'] + \
                  am_output['docker_ids'] + oc_output['docker_ids'],
    'gr_nodes': gr_output['gr_nodes'],
    'gr_db_nodes': gr_output['gr_db_nodes'],
    'op_ccm_nodes': op_output['op_ccm_nodes'],
    'op_worker_nodes': op_output['op_worker_nodes'],
    'appmock_nodes': am_output['appmock_nodes'],
    'client_nodes': oc_output['client_nodes']
}
print(json.dumps(output))
