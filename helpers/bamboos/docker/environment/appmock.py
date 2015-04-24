"""Brings up a set of appmock instances."""

import copy
import json
import os
import random
import string

import common
import docker


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(vm_args['name'], uid)
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


def up(image, bindir, dns, uid, config_path):
    config = common.parse_json_file(config_path)['appmock']
    config['config']['target_dir'] = '/root/bin'
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = common.set_up_dns(dns, uid)

    for cfg in configs:
        node_out = _node_up(image, bindir, uid, cfg, config_path, dns_servers)
        common.merge(output, node_out)

    return output
