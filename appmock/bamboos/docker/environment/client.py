# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Prepares a set dockers with oneclient instances that are configured and ready
to start.
"""

import copy
import os

from . import common, docker, dns, globalregistry, provider_worker


def client_hostname(node_name, uid):
    """Formats hostname for a docker hosting oneclient.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname(node_name, uid)


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg = {'node': cfg[name]}
    node = cfg['node']
    node['name'] = client_hostname(name, uid)
    node['op_domain'] = provider_worker.provider_domain(node['op_domain'],
                                                        uid)
    node['gr_domain'] = globalregistry.gr_domain(node['gr_domain'], uid)

    return cfg


def _node_up(image, bindir, config, config_path, dns_servers):
    node = config['node']
    hostname = node['name']

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
            'PROVIDER_HOSTNAME': node['op_domain'],
            'GLOBAL_REGISTRY_URL': node['gr_domain']}

    # We want the binary from debug more than relwithdebinfo, and any of these
    # more than from release (ifs are in reverse order so it works when
    # there are multiple dirs).
    command = '''set -e
[ -d /root/build/release ] && cp /root/build/release/oneclient /root/bin/oneclient
[ -d /root/build/relwithdebinfo ] && cp /root/build/relwithdebinfo/oneclient /root/bin/oneclient
[ -d /root/build/debug ] && cp /root/build/debug/oneclient /root/bin/oneclient
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
        name=hostname,
        hostname=hostname,
        detach=True,
        envs=envs,
        interactive=True,
        tty=True,
        workdir='/root/bin',
        volumes=[(bindir, '/root/build', 'ro')],
        dns_list=dns_servers,
        command=command)

    return {'docker_ids': [container], 'client_nodes': [hostname]}


def up(image, bindir, dns_server, uid, config_path):
    config = common.parse_json_file(config_path)['oneclient']
    configs = [_tweak_config(config, node, uid) for node in config]

    dns_servers, output = dns.maybe_start(dns_server, uid)

    for cfg in configs:
        node_out = _node_up(image, bindir, cfg, config_path, dns_servers)
        common.merge(output, node_out)

    return output
