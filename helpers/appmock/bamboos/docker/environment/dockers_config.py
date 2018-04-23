# coding=utf-8
"""Author: Lukasz Opiola
Copyright (C) 2018 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Utility module to decide which docker images should be used across bamboos
scripts. Supported image types (passed as string):

    * builder
    * worker
    * dns
    * ceph
    * s3
    * swift
    * nfs
    * glusterfs
    * riak

To override an image using a config file, place a json file called
'dockers.config' anywhere on the path from CWD to the executed bamboos script.
The file should contain simple key-value pairs, with keys being types of
dockers to use in bamboos scripts. Example 'dockers.config' content:
{
    "builder": "onedata/builder:v40",
    "worker": "onedata/worker:v42"
}

Images can also be overriden using ENV variables
(they have the highest priority):

    * BUILDER_IMAGE
    * WORKER_IMAGE
    * DNS_IMAGE
    * CEPH_IMAGE
    * S3_IMAGE
    * SWIFT_IMAGE
    * NFS_IMAGE
    * GLUSTERFS_IMAGE
    * RIAK_IMAGE

"""

import sys
import os
import json

DOCKERS_CONFIG_FILE = 'dockers.config'


def default_image(type):
    return {
        'builder': 'onedata/builder',
        'worker': 'onedata/worker',
        'dns': 'onedata/dns',
        'ceph': 'onedata/ceph',
        's3': 'onedata/s3proxy',
        'swift': 'onedata/dockswift',
        'nfs': 'erezhorev/dockerized_nfs_server',
        'glusterfs': 'gluster/gluster-centos:gluster3u7_centos7',
        'riak': 'onedata/riak'
    }[type]


def image_override_env(type):
    return {
        'builder': 'BUILDER_IMAGE',
        'worker': 'WORKER_IMAGE',
        'dns': 'DNS_IMAGE',
        'ceph': 'CEPH_IMAGE',
        's3': 'S3_IMAGE',
        'swift': 'SWIFT_IMAGE',
        'nfs': 'NFS_IMAGE',
        'glusterfs': 'GLUSTERFS_IMAGE',
        'riak': 'RIAK_IMAGE'
    }[type]


def filesystem_root():
    return os.path.abspath(os.sep)


def ensure_image(args, arg_name, type):
    if arg_name in vars(args) and vars(args)[arg_name]:
        print('Using commandline image for {}: {}'.format(type,
                                                          vars(args)[arg_name]))
        return
    else:
        vars(args)[arg_name] = get_image(type)


def get_image(type):
    if image_override_env(type) in os.environ:
        image = os.environ[image_override_env(type)]
        print('Using overriden image for {}: {}'.format(type, image))
        return image

    if os.path.isabs(sys.argv[0]):
        current_path = sys.argv[0]
        end_path = filesystem_root()
    else:
        current_path = os.path.join(os.getcwd(), sys.argv[0])
        end_path = os.getcwd()

    config_path = search_for_config(current_path, end_path)
    if config_path:
        config = json.load(open(config_path))
        if type in config:
            image = config[type]
            print('Found dockers config in {}'.format(os.path.normpath(config_path)))
            print('Using preconfigured image for {}: {}'.format(type, image))
            return image

    image = default_image(type)
    print('Using default image for {}: {}'.format(type, image))
    return image


def search_for_config(current_path, end_path):
    cfg_path = os.path.join(current_path, DOCKERS_CONFIG_FILE)
    if os.path.isfile(cfg_path):
        return cfg_path

    if current_path == end_path:
        return None

    # Step one dir upwards
    current_path = os.path.dirname(current_path)
    return search_for_config(current_path, end_path)
