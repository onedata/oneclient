# coding=utf-8
"""Author: Michal Zmuda
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a location service bootstrap node.
"""

import os
from . import common, docker

TEST_NODE_PREFIX = 'test-bootstrap-node'
PORT = '5000'


def _node_up(image, bin_oz, config, name, uid, logdir=None):
    hostname = common.format_hostname([name], uid)
    container_logs_root = '/root/log'

    rel_dir = os.path.join(bin_oz, config['dirs_config']['oz_worker']['input_dir'])
    volumes = [(rel_dir, '/root/bin', 'ro')]
    if logdir:
        logdir = os.path.join(os.path.abspath(logdir), hostname)
        os.makedirs(logdir)
        volumes.extend([(logdir, container_logs_root, 'rw')])

    cmd = '''mkdir {container_logs_root}/{hostname} && \
    node /root/bin/lib/location-service/start.js  -vvvv -p {port} -h {hostname} \
    | tee {container_logs_root}/{hostname}/debug.log'''.format(
        container_logs_root=container_logs_root,
        hostname=hostname,
        port=PORT
    )

    container = docker.run(
        image=image,
        hostname=hostname,
        name=hostname,
        volumes=volumes,
        command=cmd,
        privileged=True,
        detach=True)

    return container


def up(image, bin_oz, config, name, uid, logdir=None):
    return _node_up(image, bin_oz, config, name, uid, logdir)


def up_nodes(image, bin_oz, config, uid, logdir=None):
    used_names = set()
    docker_ids = []
    if 'zone_domains' in config:
        for zone_config in config['zone_domains'].values():
            for node_config in zone_config['oz_worker'].values():
                sys_config = node_config['sys.config']
                if 'oz_worker' in sys_config:
                    sys_config = sys_config['oz_worker']
                if 'location_service_bootstrap_nodes' in sys_config:
                    for name in sys_config['location_service_bootstrap_nodes']:
                        if name.startswith(TEST_NODE_PREFIX) and name not in used_names:
                            container = up(image, bin_oz, config, name, uid, logdir)
                            used_names.add(name)
                            docker_ids.extend([container])
    return docker_ids


def format_if_test_node(name, uid):
    return common.format_hostname([name], uid) + ":" + PORT\
        if name.startswith(TEST_NODE_PREFIX) else name
