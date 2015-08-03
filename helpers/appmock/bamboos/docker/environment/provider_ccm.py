"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider ccm nodes. They can create separate clusters.
"""

from __future__ import print_function

import copy
import json
import os

import common
import docker

import copy
import json
import os

from . import common, docker, riak, dns as dns_mod


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [common.format_nodename(n, uid) for n in
                               sys_config['ccm_nodes']]

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(name, uid)

    return cfg


def _node_up(image, bindir, logdir, uid, config, dns_servers):
    node_name = config['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
        '''mkdir -p /root/bin/node/log/
chown {uid}:{gid} /root/bin/node/log/
chmod ug+s /root/bin/node/log/
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
set -e
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/op_ccm console'''
    command = command.format(
        gen_dev_args=json.dumps({'op_ccm': config}),
        uid=os.geteuid(),
        gid=os.getegid())

    volumes = [(bindir, '/root/build', 'ro')]

    if logdir:
        logdir = os.path.join(os.path.abspath(logdir), name)
        volumes.extend([(logdir, '/root/bin/node/log', 'rw')])

    container = docker.run(
        image=image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=common.format_dockername(name, uid),
        volumes=volumes,
        dns_list=dns_servers,
        command=command)

    return (
        [container],
        {
            'docker_ids': [container],
            'op_ccm_nodes': [node_name]
        }
    )


def _ready(container):
    return True  # todo implement


def up(image, bindir, logdir, dns, uid, config_path):
    providers = common.parse_json_file(config_path)['providers']
    dns_servers, output = dns_mod.set_up_dns(dns, uid)
    # CCMs of every provider are started together
    for provider in providers:
        config = providers[provider]['op_ccm']
        config['config']['target_dir'] = '/root/bin'
        configs = [_tweak_config(config, node, uid) for node in config['nodes']]

        ccms = []
        for cfg in configs:
            ccm, node_out = _node_up(image, bindir, logdir, uid, cfg,
                                     dns_servers)
            ccms.extend(ccm)
            common.merge(output, node_out)

        common.wait_until(_ready, ccms[0:1], 0)

    return output
