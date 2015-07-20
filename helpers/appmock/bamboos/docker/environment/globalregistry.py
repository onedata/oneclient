# coding=utf-8
"""Authors: Tomasz Lichoń, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of Global Registry nodes along with databases.
They can create separate clusters.
"""

import copy
import json
import os

from . import common, docker, dns as dns_mod


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['db_nodes'] = [common.format_nodename(n, uid) for n in
                              sys_config['db_nodes']]

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}
    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_nodename(name, uid)

    return cfg


def _node_up(image, bindir, logdir, uid, config, dns_servers):
    node_name = config['nodes']['node']['vm.args']['name']
    cookie = config['nodes']['node']['vm.args']['setcookie']
    db_nodes = config['nodes']['node']['sys.config']['db_nodes']

    (gr_name, sep, gr_hostname) = node_name.partition('@')
    gr_dockername = common.format_dockername(gr_name, uid)

    gr_command = '''set -e
mkdir -p /root/bin/node/log/
chown {uid}:{gid} /root/bin/node/log/
chmod ug+s /root/bin/node/log/
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/globalregistry console'''
    gr_command = gr_command.format(
        gen_dev_args=json.dumps({'globalregistry': config}),
        uid=os.geteuid(),
        gid=os.getegid())

    # Start DB node for current GR instance.
    # Currently, only one DB node for GR is allowed, because we are using links.
    # It's impossible to create a bigcouch cluster with docker's links.
    db_node = db_nodes[0]
    (db_name, sep, db_hostname) = db_node.partition('@')
    db_dockername = common.format_dockername(db_name, uid)

    db_command = '''echo '[httpd]' > /opt/bigcouch/etc/local.ini
echo 'bind_address = 0.0.0.0' >> /opt/bigcouch/etc/local.ini
sed -i 's/-name bigcouch/-name {name}@{host}/g' /opt/bigcouch/etc/vm.args
sed -i 's/-setcookie monster/-setcookie {cookie}/g' /opt/bigcouch/etc/vm.args
/opt/bigcouch/bin/bigcouch'''
    db_command = db_command.format(name=db_name, host=db_hostname,
                                   cookie=cookie)

    bigcouch = docker.run(
        image='onedata/bigcouch',
        detach=True,
        name=db_dockername,
        hostname=db_hostname,
        command=db_command)

    volumes = [(bindir, '/root/build', 'ro')]

    if logdir:
        logdir = os.path.join(os.path.abspath(logdir), gr_name)
        volumes.extend([(logdir, '/root/bin/node/log', 'rw')])

    gr = docker.run(
        image=image,
        hostname=gr_hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=gr_dockername,
        volumes=volumes,
        dns_list=dns_servers,
        link={db_dockername: db_hostname},
        command=gr_command)

    return gr, {
        'docker_ids': [bigcouch, gr],
        'gr_db_nodes': ['{0}@{1}'.format(db_name, db_hostname)],
        'gr_nodes': ['{0}@{1}'.format(gr_name, gr_hostname)]
    }


def up(image, bindir, logdir, dns, uid, config_path):
    config = common.parse_json_file(config_path)['globalregistry']
    config['config']['target_dir'] = '/root/bin'
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = dns_mod.set_up_dns(dns, uid)

    gr_containers = []
    for cfg in configs:
        gr_container, node_out = _node_up(image, bindir, logdir, uid, cfg,
                                          dns_servers)
        common.merge(output, node_out)
        gr_containers.append(gr_container)

    return output
