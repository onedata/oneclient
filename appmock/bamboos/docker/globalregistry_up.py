#!/usr/bin/env python

"""
Brings up a set of Global Registry nodes along with databases.
They can create separate clusters.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import collections
import copy
import common
import docker
import json
import os


def tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['db_nodes'] = [common.format_hostname(n, uid) for n in sys_config['db_nodes']]

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_hostname(vm_args['name'], uid)

    return cfg


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up globalregistry nodes.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='docker image to use as a container',
    dest='image')

parser.add_argument(
    '--bin', '-b',
    action='store',
    default=os.getcwd(),
    help='path to globalregistry directory (precompiled)',
    dest='bin')

parser.add_argument(
    '--dns', '-d',
    action='store',
    default='none',
    help='IP address of DNS or "none" - if no dns should be started or ' +
         '"auto" - if it should be started automatically',
    dest='dns')

parser.add_argument(
    '--uid', '-u',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker names',
    dest='uid')

parser.add_argument(
    'config_path',
    action='store',
    help='path to gen_dev_args.json that will be used to configure the globalregistry instances')

args = parser.parse_args()
uid = args.uid

config = common.parse_json_file(args.config_path)['globalregistry']
config['config']['target_dir'] = '/root/bin'
configs = [tweak_config(config, node, uid) for node in config['nodes']]

output = collections.defaultdict(list)

(dns_servers, dns_output) = common.set_up_dns(args.dns, uid)
common.merge(output, dns_output)

for cfg in configs:
    node_name = cfg['nodes']['node']['vm.args']['name']
    cookie = cfg['nodes']['node']['vm.args']['setcookie']
    db_nodes = cfg['nodes']['node']['sys.config']['db_nodes']

    (gr_name, sep, gr_hostname) = node_name.partition('@')
    gr_dockername = common.format_dockername(gr_name, uid)

    gr_command = '''set -e
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/globalregistry console'''
    gr_command = gr_command.format(gen_dev_args=json.dumps({'globalregistry': cfg}))

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
    db_command = db_command.format(name=db_name, host=db_hostname, cookie=cookie)

    bigcouch = docker.run(
        image='onedata/bigcouch',
        detach=True,
        name=db_dockername,
        hostname=db_hostname,
        command=db_command)

    output['docker_ids'] += [bigcouch]
    output['gr_db_nodes'] += ['{0}@{1}'.format(db_name, db_hostname)]

    gr = docker.run(
        image=args.image,
        hostname=gr_hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=gr_dockername,
        volumes=[(args.bin, '/root/build', 'ro')],
        dns_list=dns_servers,
        link={db_dockername: db_hostname},
        command=gr_command)

    output['docker_ids'] += [gr]
    output['gr_nodes'] += ['{0}@{1}'.format(gr_name, gr_hostname)]

# Print JSON to output so it can be parsed by other scripts
print(json.dumps(output))
