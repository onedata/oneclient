#!/usr/bin/env python
# coding=utf-8

"""Author: Łukasz Opioła
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import json

from environment import env

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up onedata environment.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default=env.default('image'),
    help='the image to use for the components',
    dest='image')

parser.add_argument(
    '-ci', '--ceph-image',
    action='store',
    default=env.default('ceph_image'),
    help='the image to use for the ceph storages',
    dest='ceph_image')

parser.add_argument(
    '-si', '--s3-image',
    action='store',
    default=env.default('s3_image'),
    help='the image to use for the s3 storages',
    dest='s3_image')

parser.add_argument(
    '-bw', '--bin-worker',
    action='store',
    default=env.default('bin_op_worker'),
    help='the path to op_worker repository (precompiled)',
    dest='bin_op_worker')

parser.add_argument(
    '-bcw', '--bin-cluster-worker',
    action='store',
    default=env.default('bin_cluster_worker'),
    help='the path to cluster_worker repository (precompiled)',
    dest='bin_cluster_worker')

parser.add_argument(
    '-bcm', '--bin-cm',
    action='store',
    default=env.default('bin_cluster_manager'),
    help='the path to cluster_manager repository (precompiled)',
    dest='bin_cluster_manager')

parser.add_argument(
    '-boz', '--bin-oz',
    action='store',
    default=env.default('bin_oz'),
    help='the path to zone repository (precompiled)',
    dest='bin_oz')

parser.add_argument(
    '-ba', '--bin-appmock',
    action='store',
    default=env.default('bin_am'),
    help='the path to appmock repository (precompiled)',
    dest='bin_am')

parser.add_argument(
    '-bc', '--bin-client',
    action='store',
    default=env.default('bin_oc'),
    help='the path to oneclient repository (precompiled)',
    dest='bin_oc')

parser.add_argument(
    '-bl', '--bin-luma',
    action='store',
    default=env.default('bin_luma'),
    help='the path to luma repository',
    dest='bin_luma')

parser.add_argument(
    '-l', '--logdir',
    action='store',
    default=env.default('logdir'),
    help='path to a directory where the logs will be stored',
    dest='logdir')

parser.add_argument(
    'config_path',
    action='store',
    help='path to json configuration file')

args = parser.parse_args()

output = env.up(args.config_path, image=args.image, ceph_image=args.ceph_image,
                s3_image=args.s3_image, bin_am=args.bin_am,
                bin_oz=args.bin_oz,
                bin_cluster_manager=args.bin_cluster_manager,
                bin_op_worker=args.bin_op_worker,
                bin_cluster_worker=args.bin_cluster_worker,
                bin_luma=args.bin_luma,
                bin_oc=args.bin_oc, logdir=args.logdir)

print(json.dumps(output))
