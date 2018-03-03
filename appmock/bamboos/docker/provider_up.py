#!/usr/bin/env python

"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script to brings up a set of oneprovider nodes. They can create separate
clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import json
import os

from environment import common, provider_worker, cluster_manager, dns

parser = common.standard_arg_parser(
    'Bring up oneprovider nodes (workers and cms).')
parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=None,
    help='path to a directory where the logs will be stored',
    dest='logdir')
parser.add_argument(
    '-bw', '--bin-worker',
    action='store',
    default=os.getcwd(),
    help='the path to oneprovider repository (precompiled)',
    dest='bin_op_worker')
parser.add_argument(
    '-bcm', '--bin-cm',
    action='store',
    default=os.getcwd() + '/cluster_manager',
    help='the path to cluster_manager repository (precompiled)',
    dest='bin_cluster_manager')

# Prepare config
args = parser.parse_args()
config = common.parse_json_config_file(args.config_path)
output = {
    'cluster_manager_nodes': [],
    'op_worker_nodes': [],
}
uid = args.uid

# Start DNS
if args.dns == 'auto':
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(output, dns_output)
else:
    dns_server = args.dns

# Start cms
cm_output = cluster_manager.up(args.image, args.bin_cluster_manager,
                               dns_server, uid, args.config_path, args.logdir)
common.merge(output, cm_output)

# Start workers
worker_output = provider_worker.up(args.image, args.bin_op_worker, dns_server,
                                   uid, args.config_path, args.logdir)
common.merge(output, worker_output)

if dns_server != 'none':
    # Make sure domain are added to the dns server
    dns.maybe_restart_with_configuration('auto', uid, output)

# Print results
print(json.dumps(output))
