"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider cm nodes. They can create separate clusters.
"""

from __future__ import print_function

import copy
import json
import os

from . import common, docker, dns


def cm_hostname(node_name, op_instance, uid):
    """Formats hostname for a docker hosting cluster_manager.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname([node_name, op_instance], uid)


def cm_erl_node_name(node_name, op_instance, uid):
    """Formats erlang node name for a vm on cluster_manager docker.
    """
    hostname = cm_hostname(node_name, op_instance, uid)
    return common.format_erl_node_name('cm', hostname)


def _tweak_config(config, cm_node, op_instance, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][cm_node]}

    sys_config = cfg['nodes']['node']['sys.config']['cluster_manager']
    sys_config['cm_nodes'] = [cm_erl_node_name(n, op_instance, uid)
                               for n in sys_config['cm_nodes']]

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = cm_erl_node_name(cm_node, op_instance, uid)

    return cfg


def _node_up(image, bindir, config, dns_servers, logdir):
    node_name = config['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
        '''mkdir -p /root/bin/node/log/
echo 'while ((1)); do chown -R {uid}:{gid} /root/bin/node/log; sleep 1; done' > /root/bin/chown_logs.sh
bash /root/bin/chown_logs.sh &
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
set -e
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/cluster_manager console'''
    command = command.format(
        gen_dev_args=json.dumps({'cluster_manager': config}),
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

    return {
        'docker_ids': [container],
        'cluster_manager_nodes': [node_name]
    }


def _ready(container):
    return True  # todo implement


def up(image, bindir, dns_server, uid, config_path, logdir=None, domains_name='provider_domains'):
    config = common.parse_json_config_file(config_path)
    input_dir = config['dirs_config']['cluster_manager']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    # CMs of every provider are started together
    for op_instance in config[domains_name]:
        gen_dev_cfg = {
            'config': {
                'input_dir': input_dir,
                'target_dir': '/root/bin'
            },
            'nodes': config[domains_name][op_instance]['cluster_manager']
        }

        tweaked_configs = [
            _tweak_config(gen_dev_cfg, cm_node, op_instance, uid)
            for cm_node in gen_dev_cfg['nodes']]

        cms = []
        for cfg in tweaked_configs:
            node_out = _node_up(image, bindir, cfg, dns_servers, logdir)
            cms.extend(node_out['docker_ids'])
            common.merge(output, node_out)

        common.wait_until(_ready, cms, 0)

    return output
