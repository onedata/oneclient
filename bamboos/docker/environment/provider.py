"""Brings up a set of oneprovider nodes. They can create separate clusters."""

from __future__ import print_function

import copy
import json
import os
import sys
import time

import common
import docker


try:
    import xml.etree.cElementTree as eTree
except ImportError:
    import xml.etree.ElementTree as eTree

try:  # Python 2
    from urllib2 import urlopen
    from urllib2 import URLError
    from httplib import BadStatusLine
except ImportError:  # Python 3
    from urllib.request import urlopen
    from urllib.error import URLError
    from http.client import BadStatusLine


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    sys_config = cfg['nodes']['node']['sys.config']
    sys_config['ccm_nodes'] = [common.format_hostname(n, uid) for n in
                               sys_config['ccm_nodes']]

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = common.format_hostname(vm_args['name'], uid)

    return cfg


def _node_up(image, bindir, logdir, uid, config, dns_servers):
    node_type = config['nodes']['node']['sys.config']['node_type']
    node_name = config['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
        '''set -e
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/oneprovider_node console'''
    command = command.format(
        gen_dev_args=json.dumps({'oneprovider_node': config}),
        uid=os.geteuid(),
        gid=os.getegid())

    logdir = os.path.join(os.path.abspath(logdir), name)
    volumes = [(bindir, '/root/build', 'ro')]
    volumes.extend([(logdir, '/root/bin/node/log', 'rw')] if logdir else [])

    container = docker.run(
        image=image,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        name=common.format_dockername(name, uid),
        volumes=volumes,
        dns_list=dns_servers,
        command=command)

    return (
        [container] if node_type == 'ccm' else [],
        [container] if node_type == 'worker' else [],
        {
            'docker_ids': [container],
            'op_{0}_nodes'.format(node_type): [node_name]
        }
    )


def _is_up(ip):
    url = 'https://{0}/nagios'.format(ip)
    try:
        fo = urlopen(url, timeout=1)
        tree = eTree.parse(fo)
        healthdata = tree.getroot()
        status = healthdata.attrib['status']
        return status == 'ok'
    except URLError:
        return False
    except BadStatusLine:
        return False


def _wait_until_ready(workers):
    worker_ip = docker.inspect(workers[0])['NetworkSettings']['IPAddress']
    deadline = time.time() + 60
    while not _is_up(worker_ip):
        if time.time() > deadline:
            print('WARNING: did not get "ok" healthcheck status for 1 minute',
                  file=sys.stderr)
            break

        time.sleep(1)


def up(image, bindir, logdir, dns, uid, config_path):
    config = common.parse_json_file(config_path)['oneprovider_node']
    config['config']['target_dir'] = '/root/bin'
    configs = [_tweak_config(config, node, uid) for node in config['nodes']]

    dns_servers, output = common.set_up_dns(dns, uid)
    ccms = []
    workers = []

    for cfg in configs:
        ccm, worker, node_out = _node_up(image, bindir, logdir, uid, cfg,
                                         dns_servers)
        ccms.extend(ccm)
        workers.extend(worker)
        common.merge(output, node_out)

    _wait_until_ready(workers)

    if logdir:
        for node in ccms + workers:
            docker.exec_(node, ['chown', '-R',
                                '{0}:{1}'.format(os.geteuid(), os.getegid()),
                                '/root/bin/node/log/'])

    return output
