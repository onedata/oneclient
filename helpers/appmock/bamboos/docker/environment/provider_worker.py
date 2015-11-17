"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider worker nodes. They can create separate clusters.
"""

import copy
import json
import os

from . import common, docker, riak, dns, globalregistry, provider_ccm

PROVIDER_WAIT_FOR_NAGIOS_SECONDS = 60 * 2


def provider_domain(op_instance, uid):
    """Formats domain for a provider."""
    return common.format_hostname(op_instance, uid)


def worker_hostname(node_name, op_instance, uid):
    """Formats hostname for a docker hosting op_worker.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname([node_name, op_instance], uid)


def worker_erl_node_name(node_name, op_instance, uid):
    """Formats erlang node name for a vm on op_worker docker.
    """
    hostname = worker_hostname(node_name, op_instance, uid)
    return common.format_erl_node_name('worker', hostname)


def _tweak_config(config, name, op_instance, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [
        provider_ccm.ccm_erl_node_name(n, op_instance, uid) for n in
        sys_config['ccm_nodes']]
    # Set the provider domain (needed for nodes to start)
    sys_config['provider_domain'] = provider_domain(op_instance, uid)

    if 'global_registry_domain' in sys_config:
        gr_hostname = globalregistry.gr_domain(
            sys_config['global_registry_domain'], uid)
        sys_config['global_registry_domain'] = gr_hostname
    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = worker_erl_node_name(name, op_instance, uid)

    return cfg, sys_config['db_nodes']


def _node_up(image, bindir, config, dns_servers, db_node_mappings, logdir):
    node_name = config['nodes']['node']['vm.args']['name']
    db_nodes = config['nodes']['node']['sys.config']['db_nodes']
    for i in range(len(db_nodes)):
        db_nodes[i] = db_node_mappings[db_nodes[i]]

    (name, sep, hostname) = node_name.partition('@')

    command = '''mkdir -p /root/bin/node/log/
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
        logdir = os.path.join(os.path.abspath(logdir), hostname)
        volumes.extend([(logdir, '/root/bin/node/log', 'rw')])

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

    return container, {
        'docker_ids': [container],
        'op_worker_nodes': [node_name]
    }


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
    riak_output = riak.up('onedata/riak', dns, uid, None, cluster_name,
                          len(db_node_mappings))

    return db_node_mappings, riak_output


def up(image, bindir, dns_server, uid, config_path, logdir=None):
    config = common.parse_json_file(config_path)
    input_dir = config['dirs_config']['op_worker']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    # Workers of every provider are started together
    for op_instance in config['provider_domains']:
        gen_dev_cfg = {
            'config': {
                'input_dir': input_dir,
                'target_dir': '/root/bin'
            },
            'nodes': config['provider_domains'][op_instance]['op_worker']
        }

        # Tweak configs, retrieve lis of riak nodes to start
        configs = []
        riak_nodes = []
        for worker_node in gen_dev_cfg['nodes']:
            tw_cfg, db_nodes = _tweak_config(gen_dev_cfg, worker_node,
                                             op_instance, uid)
            configs.append(tw_cfg)
            riak_nodes.extend(db_nodes)

        # Start riak nodes, obtain mappings
        db_node_mappings, riak_out = _riak_up(op_instance, riak_nodes,
                                              dns_servers, uid)
        common.merge(output, riak_out)

        # Start the workers
        workers = []
        worker_ips = []
        for cfg in configs:
            worker, node_out = _node_up(image, bindir, cfg,
                                        dns_servers, db_node_mappings, logdir)
            workers.append(worker)
            worker_ips.append(common.get_docker_ip(worker))
            common.merge(output, node_out)

        # Wait for all workers to start
        common.wait_until(_ready, workers, PROVIDER_WAIT_FOR_NAGIOS_SECONDS)

        # Add the domain of current providers
        domains = {
            'domains': {
                provider_domain(op_instance, uid): {
                    'ns': worker_ips,
                    'a': []
                }
            }
        }
        common.merge(output, domains)

    # Make sure domains are added to the dns server.
    dns.maybe_restart_with_configuration(dns_server, uid, output)
    return output
