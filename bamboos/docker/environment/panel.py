"""Brings up a set of onepanel nodes. They can create separate clusters."""

from __future__ import print_function

import copy
import json
import os
import sys
import time

import common
import docker


try:
    import xml.etree.cElementTree as eTree
except ImportError:
    import xml.etree.ElementTree as eTree

try:  # Python 2
    from urllib2 import urlopen
    from urllib2 import URLError
    from httplib import BadStatusLine
except ImportError:  # Python 3
    from urllib.request import urlopen
    from urllib.error import URLError
    from http.client import BadStatusLine


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(vm_args['name'], uid)

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['application_release_path'] = '/tmp/release'

    return cfg


def _node_up(image, bindir, uid, config, dns_servers, release_path,
             storage_paths):
    node_name = config['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
        '''set -e
cp -R /root/release /tmp/release
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/onepanel console'''
    command = command.format(
        gen_dev_args=json.dumps({'onepanel': config}),
        uid=os.geteuid(),
        gid=os.getegid())

    volumes = [(bindir, '/root/build', 'ro'),
               (release_path, '/root/release', 'ro')]

    for storage_path in storage_paths:
        volumes.append((storage_path, storage_path, 'rw'))

    container = docker.run(
        image=image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=common.format_dockername(node_name, uid),
        volumes=volumes,
        dns_list=dns_servers,
        command=command)

    return (
        {
            'docker_ids': [container],
            'onepanel_nodes': [node_name]
        }
    )


def up(image, bindir, dns, uid, config_path, release_path, storage_paths):
    config = common.parse_json_file(config_path)['onepanel']
    config['config']['target_dir'] = '/root/bin'
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = common.set_up_dns(dns, uid)

    for cfg in configs:
        node_out = _node_up(image, bindir, uid, cfg, dns_servers, release_path,
                            storage_paths)
        common.merge(output, node_out)

    return output
