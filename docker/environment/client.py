"""Prepares a set dockers with oneclient instances that are configured and ready
to start.
"""

import copy
import os

import common
import docker


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}
    node = cfg['nodes']['node']
    node['name'] = common.format_nodename(node['name'], uid)
    node['op_hostname'] = common.format_nodename(node['op_hostname'], uid)
    node['gr_hostname'] = common.format_nodename(node['gr_hostname'], uid)

    return cfg


def _node_up(image, bindir, uid, config, config_path, dns_servers):
    node = config['nodes']['node']
    node_name = node['name']
    (name, sep, hostname) = node_name.partition('@')

    cert_file_path = node['user_cert']
    key_file_path = node['user_key']
    # cert_file_path and key_file_path can both be an absolute path
    # or relative to gen_dev_args.json
    cert_file_path = os.path.join(common.get_file_dir(config_path),
                                  cert_file_path)
    key_file_path = os.path.join(common.get_file_dir(config_path),
                                 key_file_path)

    node['user_cert'] = '/tmp/cert'
    node['user_key'] = '/tmp/key'

    envs = {'X509_USER_CERT': node['user_cert'],
            'X509_USER_KEY': node['user_key'],
            'PROVIDER_HOSTNAME': node['op_hostname'],
            'GLOBAL_REGISTRY_URL': node['gr_hostname']}

    command = '''set -e
cp /root/build/release/oneclient /root/bin/oneclient
cat <<"EOF" > /tmp/cert
{cert_file}
EOF
cat <<"EOF" > /tmp/key
{key_file}
EOF
bash'''
    command = command.format(
        cert_file=open(cert_file_path, 'r').read(),
        key_file=open(key_file_path, 'r').read())

    container = docker.run(
        image=image,
        hostname=hostname,
        detach=True,
        envs=envs,
        interactive=True,
        tty=True,
        workdir='/root/bin',
        name=common.format_dockername(name, uid),
        volumes=[(bindir, '/root/build', 'ro')],
        dns_list=dns_servers,
        command=command)

    return {'docker_ids': [container], 'client_nodes': [hostname]}


def up(image, bindir, dns, uid, config_path):
    config = common.parse_json_file(config_path)['oneclient']
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = common.set_up_dns(dns, uid)

    for cfg in configs:
        node_out = _node_up(image, bindir, uid, cfg, config_path, dns_servers)
        common.merge(output, node_out)

    return output