"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider ccm nodes. They can create separate clusters.
"""

from __future__ import print_function

import copy
import json
import os

from . import common, docker, dns


def ccm_hostname(node_name, op_instance, uid):
    """Formats hostname for a docker hosting op_ccm.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname([node_name, op_instance], uid)


def ccm_erl_node_name(node_name, op_instance, uid):
    """Formats erlang node name for a vm on op_ccm docker.
    """
    hostname = ccm_hostname(node_name, op_instance, uid)
    return common.format_erl_node_name('ccm', hostname)


def _tweak_config(config, ccm_node, op_instance, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][ccm_node]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [ccm_erl_node_name(n, op_instance, uid)
                               for n in sys_config['ccm_nodes']]

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = ccm_erl_node_name(ccm_node, op_instance, uid)

    return cfg


def _node_up(image, bindir, config, dns_servers, logdir):
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
        'op_ccm_nodes': [node_name]
    }


def _ready(container):
    return True  # todo implement


def up(image, bindir, dns_server, uid, config_path, logdir=None):
    config = common.parse_json_file(config_path)
    input_dir = config['dirs_config']['op_ccm']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    # CCMs of every provider are started together
    for op_instance in config['provider_domains']:
        gen_dev_cfg = {
            'config': {
                'input_dir': input_dir,
                'target_dir': '/root/bin'
            },
            'nodes': config['provider_domains'][op_instance]['op_ccm']
        }

        tweaked_configs = [
            _tweak_config(gen_dev_cfg, ccm_node, op_instance, uid)
            for ccm_node in gen_dev_cfg['nodes']]

        ccms = []
        for cfg in tweaked_configs:
            node_out = _node_up(image, bindir, cfg, dns_servers, logdir)
            ccms.extend(node_out['docker_ids'])
            common.merge(output, node_out)

        common.wait_until(_ready, ccms, 0)

    return output
