#!/usr/bin/env python

"""
Brings up a oneprovider cluster.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import collections
import copy
import docker
import json
import os
import time

def parse_config(path):
    with open(path, 'r') as f:
        data = f.read()
        return json.loads(data)

def set_hostname(node, uid):
    parts = list(node.partition('@'))
    parts[2] = '{0}.{1}.dev.docker'.format(parts[0], uid)
    return ''.join(parts)

def tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = { 'node': cfg['nodes'][name] }

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [set_hostname(n, uid) for n in sys_config['ccm_nodes']]
    sys_config['db_nodes']  = [set_hostname(n, uid) for n in sys_config['db_nodes']]

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = set_hostname(vm_args['name'], uid)

    return cfg


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up oneprovider cluster.')

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
    help='path to oneprovider directory (precompiled)',
    dest='bin')

parser.add_argument(
    '--create-service', '-c',
    action='store',
    default='{0}/createService.js'.format(os.path.dirname(os.path.realpath(__file__))),
    help='path to createService.js plugin',
    dest='create_service')

parser.add_argument(
    'config_path',
    action='store',
    help='path to gen_dev_args.json that will be used to configure the cluster')

args = parser.parse_args()
uid = str(int(time.time()))

config = parse_config(args.config_path)['oneprovider_node']
config['config']['target_dir'] = '/root/bin'
configs = [tweak_config(config, node, uid) for node in config['nodes']]

skydns = docker.run(
    image='crosbymichael/skydns',
    detach=True,
    name='skydns_{0}'.format(uid),
    command=['-nameserver', '8.8.8.8:53', '-domain', 'docker'])

skydock = docker.run(
    image='crosbymichael/skydock',
    detach=True,
    name='skydock_{0}'.format(uid),
    reflect=[('/var/run/docker.sock', 'rw')],
    volumes=[(args.create_service, '/createService.js', 'ro')],
    command=['-ttl', '30', '-environment', 'dev', '-s', '/var/run/docker.sock',
             '-domain', 'docker', '-name', 'skydns_{0}'.format(uid), '-plugins',
             '/createService.js'])

skydns_config = docker.inspect(skydns)
dns = skydns_config['NetworkSettings']['IPAddress']

output = collections.defaultdict(list)
output['dns'] = dns
output['docker_ids'] = [skydns, skydock]

for cfg in configs:
    node_type = cfg['nodes']['node']['sys.config']['node_type']
    node_name = cfg['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
    '''set -e
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/oneprovider_node console'''
    command = command.format(gen_dev_args=json.dumps({'oneprovider_node': cfg}))

    container = docker.run(
        image=args.image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name='{0}_{1}'.format(name, uid),
        volumes=[(args.bin, '/root/build', 'ro')],
        dns=[dns],
        command=command)

    output['docker_ids'].append(container)
    output['op_{type}_nodes'.format(type=node_type)].append(node_name)

print(json.dumps(output))
