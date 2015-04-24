#!/usr/bin/env python

"""
Brings up a set of appmock instances.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import collections
import common
import copy
import docker
import json
import os
import random
import string


def tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_hostname(vm_args['name'], uid)
    # Set random cookie so the node does not try to connect to others
    vm_args['setcookie'] = ''.join(random.sample(string.ascii_letters + string.digits, 16))

    return cfg


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up appmock nodes.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='the image to use for the container',
    dest='image')

parser.add_argument(
    '--bin', '-b',
    action='store',
    default=os.getcwd(),
    help='the path to appmock repository (precompiled)',
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
    help='path to gen_dev_args.json that will be used to configure appmock instances')

args = parser.parse_args()
uid = args.uid

config = common.parse_json_file(args.config_path)['appmock']
config['config']['target_dir'] = '/root/bin'
configs = [tweak_config(config, node, uid) for node in config['nodes']]

output = collections.defaultdict(list)

(dns_servers, dns_output) = common.set_up_dns(args.dns, uid)
common.merge(output, dns_output)

for cfg in configs:
    node_name = cfg['nodes']['node']['vm.args']['name']
    (name, sep, hostname) = node_name.partition('@')

    sys_config = cfg['nodes']['node']['sys.config']
    app_desc_file_path = sys_config['app_description_file']
    app_desc_file_name = os.path.basename(app_desc_file_path)
    # app_desc_file_path can be an absolute path or relative to gen_dev_args.json
    app_desc_file_path = os.path.join(common.get_file_dir(args.config_path), app_desc_file_path)

    # app_desc_file_name must be preserved, as it is an .erl file and its module name
    # must match its file name.
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
        gen_dev_args=json.dumps({'appmock': cfg}))

    container = docker.run(
        image=args.image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=common.format_dockername(name, uid),
        volumes=[(args.bin, '/root/build', 'ro')],
        dns_list=dns_servers,
        command=command)

    output['docker_ids'].append(container)
    output['appmock_nodes'].append(node_name)

# Print JSON to output so it can be parsed by other scripts
print(json.dumps(output))
