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

from . import common, docker, dns as dns_mod


APPMOCK_WAIT_FOR_NAGIOS_SECONDS = 60 * 5


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(name, uid)
    # Set random cookie so the node does not try to connect to others
    vm_args['setcookie'] = ''.join(
        random.sample(string.ascii_letters + string.digits, 16))

    return cfg


def _node_up(image, bindir, uid, config, config_path, dns_servers):
    node_name = config['nodes']['node']['vm.args']['name']
    (name, sep, hostname) = node_name.partition('@')

    sys_config = config['nodes']['node']['sys.config']
    # can be an absolute path or relative to gen_dev_args.json
    app_desc_file_path = sys_config['app_description_file']
    app_desc_file_name = os.path.basename(app_desc_file_path)
    app_desc_file_path = os.path.join(common.get_file_dir(config_path),
                                      app_desc_file_path)

    # file_name must be preserved as it must match the Erlang module name
    sys_config['app_description_file'] = '/tmp/' + app_desc_file_name

    command = '''set -e
cat <<"EOF" > /tmp/{app_desc_file_name}
{app_desc_file}
EOF
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/appmock console'''
    command = command.format(
        app_desc_file_name=app_desc_file_name,
        app_desc_file=open(app_desc_file_path, 'r').read(),
        gen_dev_args=json.dumps({'appmock': config}))

    container = docker.run(
        image=image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=common.format_dockername(name, uid),
        volumes=[(bindir, '/root/build', 'ro')],
        dns_list=dns_servers,
        command=command)

    return {'docker_ids': [container], 'appmock_nodes': [node_name]}


def _ready(node):
    node_ip = docker.inspect(node)['NetworkSettings']['IPAddress']
    return common.nagios_up(node_ip, '9999')


def up(image, bindir, dns, uid, config_path):
    config = common.parse_json_file(config_path)['appmock']
    config['config']['target_dir'] = '/root/bin'
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = dns_mod.set_up_dns(dns, uid)
    appmock_node_ids = []

    for cfg in configs:
        node_out = _node_up(image, bindir, uid, cfg, config_path, dns_servers)
        appmock_node_ids.extend(node_out['docker_ids'])
        common.merge(output, node_out)

    common.wait_until(_ready, appmock_node_ids, APPMOCK_WAIT_FOR_NAGIOS_SECONDS)

    return output
