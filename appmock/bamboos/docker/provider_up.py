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

from environment import common, provider_worker, provider_ccm, dns as dns_mod

parser = common.standard_arg_parser('Bring up oneprovider nodes (workers and ccms).')
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
    '-bccm', '--bin-ccm',
    action='store',
    default=os.getcwd() + '/op_ccm',
    help='the path to op_ccm repository (precompiled)',
    dest='bin_op_ccm')

# Prepare config
args = parser.parse_args()
config = common.parse_json_file(args.config_path)
output = {
    'op_ccm_nodes': [],
    'op_worker_nodes': [],
}
uid = common.generate_uid()

# Start DNS
[dns], dns_output = dns_mod.set_up_dns('auto', uid)

# Start cluster
ccm_output = provider_ccm.up(args.image, args.bin_op_ccm, args.logdir, dns, uid,
                             args.config_path)
worker_output = provider_worker.up(args.image, args.bin_op_worker, args.logdir,
                                   dns, uid, args.config_path)

# Print results
common.merge(output, dns_output)
common.merge(output, ccm_output)
common.merge(output, worker_output)
print(json.dumps(output))
