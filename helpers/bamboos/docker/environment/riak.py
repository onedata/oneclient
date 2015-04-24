"""Brings up a riak cluster."""

from __future__ import print_function
import re
import sys
import time

import common
import docker


RIAK_READY_WAIT_SECONDS = 60 * 5


def _riak(num):
    return 'riak{0}'.format(num)


def config_entry(num, uid):
    return '{0}:8087'.format(common.format_hostname(_riak(num), uid))


def _node_up(command, num, maps, dns, image, uid):
    hostname = common.format_hostname(_riak(num), uid)
    node = docker.run(
        image=image,
        name=common.format_dockername(_riak(num), uid),
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        dns_list=dns,
        command=command.format(maps=maps, hostname=hostname))

    return {
        'docker_ids': [node],
        'riak_nodes': [hostname]
    }


def _ready(container):
    return docker.exec_(container, ['riak', 'ping'], stdout=sys.stderr) == 0


def _ring_ready(container):
    output = docker.exec_(container, ['riak-admin', 'ring_status'], output=True,
                          stdout=sys.stderr)
    return bool(re.search('Ring Ready:\s*true', output))


def _bucket_ready(bucket, container):
    output = docker.exec_(container,
                          ['riak-admin', 'bucket-type', 'status', 'maps'],
                          output=True, stdout=sys.stderr)
    return '{0} has been created and may be activated'.format(bucket) in output


def _wait_until(condition, containers):
    deadline = time.time() + RIAK_READY_WAIT_SECONDS
    for container in containers:
        while not condition(container):
            if time.time() > deadline:
                print("WARNING: timeout while waiting for Riak",
                      file=sys.stderr)
                break

            time.sleep(1)


def _cluster_nodes(containers, uid):
    for container in containers[1:]:
        docker.exec_(
            container,
            ['riak-admin', 'cluster', 'join',
             'riak@{0}'.format(common.format_hostname(_riak(0), uid))],
            stdout=sys.stderr)

    _wait_until(_ring_ready, containers)
    docker.exec_(containers[0], ['riak-admin', 'cluster', 'plan'],
                 stdout=sys.stderr)
    docker.exec_(containers[0], ['riak-admin', 'cluster', 'commit'],
                 stdout=sys.stderr)


def up(image, dns, uid, maps, nodes):
    if not maps:
        maps = '{"props":{"n_val":2, "datatype":"map"}}'

    dns_servers, dns_output = common.set_up_dns(dns, uid)
    riak_output = {}

    command = '''
sed -i 's/riak@127.0.0.1/riak@{hostname}/' /etc/riak/riak.conf
sed -i 's/127.0.0.1:/0.0.0.0:/' /etc/riak/riak.conf
riak console'''

    for num in range(nodes):
        node_out = _node_up(command, num, maps, dns_servers, image, uid)
        common.merge(riak_output, node_out)

    containers = riak_output['docker_ids']
    common.merge(riak_output, dns_output)

    _wait_until(_ready, containers)
    _cluster_nodes(containers, uid)

    docker.exec_(containers[0],
                 command=['riak-admin', 'bucket-type', 'create', 'maps', maps],
                 stdout=sys.stderr)

    _wait_until(lambda c: _bucket_ready('maps', c), containers[:1])

    docker.exec_(containers[0],
                 command=['riak-admin', 'bucket-type', 'activate', 'maps'],
                 stdout=sys.stderr)

    common.merge(riak_output, dns_output)
    return riak_output
