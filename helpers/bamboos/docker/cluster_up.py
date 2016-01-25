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
import os
from environment import common, cluster_worker, cluster_manager, dns

parser = common.standard_arg_parser(
    'Bring up bare cluster nodes (workers and cms).')
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
    help='the path to cluster-worker repository (precompiled)',
    dest='bin_op_worker')
parser.add_argument(
    '-bcm', '--bin-cm',
    action='store',
    default=os.getcwd() + '/cluster_manager',
    help='the path to cluster_manager repository (precompiled)',
    dest='bin_cluster_manager')

# Prepare config
args = parser.parse_args()
config = common.parse_json_file(args.config_path)
output = {
    'cluster_manager_nodes': [],
    'cluster_worker_nodes': [],
}
uid = common.generate_uid()

# Start DNS
[dns_server], dns_output = dns.maybe_start('auto', uid)
common.merge(output, dns_output)

# Start cms
cm_output = cluster_manager.up(args.image, args.bin_cluster_manager, dns_server, uid, args.config_path, args.logdir)
common.merge(output, cm_output)

# Start workers
worker_output = cluster_worker.up(args.image, args.bin_op_worker, dns_server, uid, args.config_path, args.logdir)
common.merge(output, worker_output)

# Make sure domain are added to the dns server
dns.maybe_restart_with_configuration('auto', uid, output)

# Print results
print(json.dumps(output))
