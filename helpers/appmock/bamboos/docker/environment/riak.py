"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a riak cluster.
"""

from __future__ import print_function

import re
import requests
import sys

from . import common, docker, dns as dns_mod

RIAK_READY_WAIT_SECONDS = 60 * 5


def riak_hostname(node_num, op_instance, uid):
    """Formats hostname for a docker hosting riak.
    NOTE: Hostnames are also used as docker names!
    """
    node_name = 'riak{0}'.format(node_num)
    return common.format_hostname([node_name, op_instance], uid)


def riak_erl_node_name(node_name, op_instance, uid):
    """Formats erlang node name for a vm on cluster_manager docker.
    """
    hostname = riak_hostname(node_name, op_instance, uid)
    return common.format_erl_node_name('riak', hostname)


def config_entry(cluster_name, node_num, uid):
    return '{0}:8087'.format(riak_hostname(node_num, cluster_name, uid))


def _node_up(command, cluster_name, node_num, maps, dns, image, uid):
    hostname = riak_hostname(node_num, cluster_name, uid)
    node = docker.run(
        image=image,
        name=hostname,
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
    ip = docker.inspect(container)['NetworkSettings']['IPAddress']
    url = 'http://{0}:8098/stats'.format(ip)
    try:
        r = requests.head(url, timeout=5)
        return r.status_code == requests.codes.ok
    except requests.ConnectionError:
        return False


def _ring_ready(container):
    output = docker.exec_(container, ['riak-admin', 'ring_status'], output=True,
                          stdout=sys.stderr)
    return bool(re.search(r'Ring Ready:\s*true', output))


def _bucket_ready(bucket, container):
    output = docker.exec_(container,
                          ['riak-admin', 'bucket-type', 'status', 'maps'],
                          output=True, stdout=sys.stderr)
    return '{0} has been created and may be activated'.format(bucket) in output


def _admin_test_ready(container):
    result = docker.exec_(container, ['riak-admin', 'test'], stdout=sys.stderr)
    return result == 0


def _wait_until(condition, containers):
    common.wait_until(condition, containers, RIAK_READY_WAIT_SECONDS)


def _cluster_nodes(cluster_name, containers, uid):
    for container in containers[1:]:
        docker.exec_(
            container,
            ['riak-admin', 'cluster', 'join',
             'riak@{0}'.format(riak_hostname(0, cluster_name, uid))],
            stdout=sys.stderr)

    _wait_until(_ring_ready, containers)
    docker.exec_(containers[0], ['riak-admin', 'cluster', 'plan'],
                 stdout=sys.stderr)
    docker.exec_(containers[0], ['riak-admin', 'cluster', 'commit'],
                 stdout=sys.stderr)


def up(image, dns, uid, maps, cluster_name, nodes):
    if not maps:
        maps = '{"props":{"n_val":2, "datatype":"map"}}'

    dns_servers, dns_output = dns_mod.maybe_start(dns, uid)
    riak_output = {}

    command = '''
sed -i 's/riak@127.0.0.1/riak@{hostname}/' /etc/riak/riak.conf
sed -i 's/127.0.0.1:/0.0.0.0:/g' /etc/riak/riak.conf
riak console'''

    for node_num in range(nodes):
        node_out = _node_up(command, cluster_name, node_num, maps, dns_servers,
                            image, uid)
        common.merge(riak_output, node_out)

    containers = riak_output['docker_ids']
    common.merge(riak_output, dns_output)

    _wait_until(_ready, containers)

    if len(containers) > 1:
        _cluster_nodes(cluster_name, containers, uid)

    docker.exec_(containers[0],
                 command=['riak-admin', 'bucket-type', 'create', 'maps', maps],
                 stdout=sys.stderr)

    _wait_until(lambda c: _bucket_ready('maps', c), containers[:1])

    docker.exec_(containers[0],
                 command=['riak-admin', 'bucket-type', 'activate', 'maps'],
                 stdout=sys.stderr)

    _wait_until(_admin_test_ready, containers)

    common.merge(riak_output, dns_output)
    return riak_output
