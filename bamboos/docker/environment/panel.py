# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of onepanel nodes. They can create separate clusters.
"""

import copy
import json
import os

from . import common, docker, dns


def panel_hostname(node_name, uid):
    """Formats hostname for a docker hosting onepanel.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname(node_name, uid)


def panel_erl_node_name(node_name, uid):
    """Formats erlang node name for a vm on onepanel docker.
    """
    hostname = panel_hostname(node_name, uid)
    return common.format_erl_node_name('onepanel', hostname)


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = panel_erl_node_name(name, uid)

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['application_release_path'] = '/tmp/release'

    return cfg


def _node_up(image, bindir, config, dns_servers, release_path,
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
/root/bin/node/bin/op_panel console'''
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
        name=hostname,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        volumes=volumes,
        dns_list=dns_servers,
        command=command)

    return (
        {
            'docker_ids': [container],
            'onepanel_nodes': [node_name]
        }
    )


def up(image, bindir, dns_server, uid, config_path, release_path,
       storage_paths):
    config = common.parse_json_file(config_path)
    input_dir = config['dirs_config']['onepanel']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    gen_dev_cfg = {
        'config': {
            'input_dir': input_dir,
            'target_dir': '/root/bin'
        },
        'nodes': config['onepanel']
    }

    configs = [_tweak_config(gen_dev_cfg, node, uid)
               for node in gen_dev_cfg['nodes']]

    for cfg in configs:
        node_out = _node_up(image, bindir, cfg, dns_servers,
                            release_path, storage_paths)
        common.merge(output, node_out)

    return output
