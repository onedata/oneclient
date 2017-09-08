# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of appmock instances.
"""

import copy
import json
import os
import random
import string
from timeouts import *

from . import common, docker, dns, cluster_manager, worker


def domain(appmock_instance, uid):
    """Formats domain for an appmock instance.
    It is intended to fake OP or OZ domain.
    """
    return common.format_hostname(appmock_instance, uid)


def appmock_hostname(node_name, uid):
    """Formats hostname for a docker hosting appmock.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname(node_name, uid)


def appmock_erl_node_name(node_name, uid):
    """Formats erlang node name for a vm on appmock docker.
    """
    hostname = appmock_hostname(node_name, uid)
    return common.format_erl_node_name('appmock', hostname)


def _tweak_config(config, appmock_node, appmock_instance, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][appmock_node]}
    mocked_app = 'none'
    if 'mocked_app' in cfg['nodes']['node']:
        mocked_app = cfg['nodes']['node']['mocked_app']

    # Node name depends on mocked app, if none is specified,
    # default appmock_erl_node_name will be used.
    node_name = {
        'cluster_manager': cluster_manager.cm_erl_node_name(appmock_node,
                                                            appmock_instance,
                                                            uid),
        'op_worker': worker.worker_erl_node_name(appmock_node,
                                                 appmock_instance,
                                                 uid),
        'oz_worker': worker.worker_erl_node_name(appmock_node, appmock_instance, uid)
    }.get(mocked_app, appmock_erl_node_name(appmock_node, uid))

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = node_name
    # If cookie is not specified, set random cookie
    # so the node does not try to connect to others
    if 'setcookie' not in vm_args:
        vm_args['setcookie'] = ''.join(
            random.sample(string.ascii_letters + string.digits, 16))

    return cfg


def _node_up(image, bindir, config, config_path, dns_servers, logdir):
    node_name = config['nodes']['node']['vm.args']['name']
    (name, sep, hostname) = node_name.partition('@')

    sys_config = config['nodes']['node']['sys.config']['appmock']
    # can be an absolute path or relative to gen_dev_args.json
    app_desc_file_path = sys_config['app_description_file']
    app_desc_file_name = os.path.basename(app_desc_file_path)
    app_desc_file_path = os.path.join(common.get_file_dir(config_path),
                                      app_desc_file_path)

    # file_name must be preserved as it must match the Erlang module name
    sys_config['app_description_file'] = '/tmp/' + app_desc_file_name

    command = '''mkdir -p /root/bin/node/log/
bindfs --create-for-user={uid} --create-for-group={gid} /root/bin/node/log /root/bin/node/log
set -e
cat <<"EOF" > /tmp/{app_desc_file_name}
{app_desc_file}
EOF
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/appmock console
sleep 5'''  # Add sleep so logs can be chowned
    command = command.format(
        uid=os.geteuid(),
        gid=os.getegid(),
        app_desc_file_name=app_desc_file_name,
        app_desc_file=open(app_desc_file_path, 'r').read(),
        gen_dev_args=json.dumps({'appmock': config}))

    bindir = os.path.abspath(bindir)
    volumes = ['/root/bin', (bindir, bindir, 'ro')]

    if logdir:
        logdir = os.path.join(os.path.abspath(logdir), hostname)
        os.makedirs(logdir)
        volumes.extend([(logdir, '/root/bin/node/log', 'rw')])

    container = docker.run(
        image=image,
        name=hostname,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir=bindir,
        volumes=volumes,
        dns_list=dns_servers,
        privileged=True,
        command=command)

    return container, {
        'docker_ids': [container],
        'appmock_nodes': [node_name]
    }


def _ready(node):
    node_ip = docker.inspect(node)['NetworkSettings']['IPAddress']
    return common.nagios_up(node_ip, '9999')


def up(image, bindir, dns_server, uid, config_path, logdir=None):
    config = common.parse_json_config_file(config_path)
    input_dir = config['dirs_config']['appmock']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    for appmock_instance in config['appmock_domains']:
        gen_dev_cfg = {
            'config': {
                'input_dir': input_dir,
                'target_dir': '/root/bin'
            },
            'nodes': config['appmock_domains'][appmock_instance]['appmock']
        }

        tweaked_configs = [_tweak_config(gen_dev_cfg, appmock_node,
                                         appmock_instance, uid)
                           for appmock_node in gen_dev_cfg['nodes']]

        include_domain = False
        appmock_ips = []
        appmocks = []
        for cfg in tweaked_configs:
            appmock_id, node_out = _node_up(image, bindir, cfg,
                                            config_path, dns_servers, logdir)
            appmocks.append(appmock_id)
            if 'mocked_app' in cfg['nodes']['node']:
                mocked_app = cfg['nodes']['node']['mocked_app']
                if mocked_app == 'op_worker' or mocked_app == 'oz_worker':
                    include_domain = True
                    appmock_ips.append(common.get_docker_ip(appmock_id))
            common.merge(output, node_out)

        common.wait_until(_ready, appmocks, APPMOCK_WAIT_FOR_NAGIOS_SECONDS)

        if include_domain:
            domains = {
                'domains': {
                    domain(appmock_instance, uid): {
                        'ns': [],
                        'a': appmock_ips
                    }
                }
            }
            common.merge(output, domains)

    # Make sure domain are added to the dns server
    dns.maybe_restart_with_configuration(dns_server, uid, output)
    return output
