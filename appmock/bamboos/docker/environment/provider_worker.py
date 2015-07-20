"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider worker nodes. They can create separate clusters.
"""

import copy
import json
import os

from . import common, docker, riak, dns as dns_mod

PROVIDER_WAIT_FOR_NAGIOS_SECONDS = 60


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [common.format_nodename(n, uid) for n in
                               sys_config['ccm_nodes']]

    if 'global_registry_node' in sys_config:
        sys_config['global_registry_node'] = \
            common.format_hostname(sys_config['global_registry_node'], uid)

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(name, uid)

    return cfg, sys_config['db_nodes']


def _node_up(image, bindir, logdir, uid, config, dns_servers, db_node_mappings):
    node_name = config['nodes']['node']['vm.args']['name']
    db_nodes = config['nodes']['node']['sys.config']['db_nodes']
    for i in range(len(db_nodes)):
        db_nodes[i] = db_node_mappings[db_nodes[i]]

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
/root/bin/node/bin/op_worker console'''
    command = command.format(
        gen_dev_args=json.dumps({'op_worker': config}),
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
            'op_worker_nodes': [node_name]
        }
    )


def _ready(container):
    ip = docker.inspect(container)['NetworkSettings']['IPAddress']
    return common.nagios_up(ip)


def _riak_up(cluster_name, riak_nodes, dns_servers, uid):
    db_node_mappings = {}
    for node in riak_nodes:
        db_node_mappings[node] = ''

    i = 0
    for node in iter(db_node_mappings.keys()):
        db_node_mappings[node] = riak.config_entry(cluster_name, i, uid)
        i += 1

    if i == 0:
        return db_node_mappings, {}

    [dns] = dns_servers
    riak_output = riak.up('onedata/riak', dns, uid, None, cluster_name, len(db_node_mappings))

    return db_node_mappings, riak_output


def up(image, bindir, logdir, dns, uid, config_path):
    providers = common.parse_json_file(config_path)['providers']
    dns_servers, output = dns_mod.set_up_dns(dns, uid)
    # Workers of every provider are started together
    for provider in providers:
        config = providers[provider]['op_worker']
        config['config']['target_dir'] = '/root/bin'
        tweaked_configs = [_tweak_config(config, node, uid) for node in config['nodes']]
        configs = []
        riak_nodes = []
        for tw_cfg, db_nodes in tweaked_configs:
            configs.append(tw_cfg)
            riak_nodes += db_nodes

        workers = []

        db_node_mappings, riak_out = _riak_up(provider, riak_nodes, dns_servers, uid)
        common.merge(output, riak_out)

        for cfg in configs:
            worker, node_out = _node_up(image, bindir, logdir, uid, cfg,
                                        dns_servers, db_node_mappings)
            workers.extend(worker)
            common.merge(output, node_out)

        common.wait_until(_ready, workers[0:1], PROVIDER_WAIT_FOR_NAGIOS_SECONDS)

    return output
