"""Author: Rafal Slota
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a couchbase cluster.
"""

from __future__ import print_function

import re
import requests
import sys
import time
from timeouts import *

from . import common, docker, dns as dns_mod

ADMIN_PORT = 8091
CLIENT_PROXY_PORT = 11211
ALL_COUCHBASE_PORTS = [
    8091, 8092, 8093, 8094, 11207, 11210, 11211, 18091, 18092, 18093
]


def _couchbase(cluster_name, num):
    return 'couchbase{0}-{1}'.format(num, cluster_name)


def config_entry(cluster_name, num, uid, docker_host=None):
    if docker_host:
        hostname = docker_host['ssh_hostname']
    else:
        hostname = common.format_hostname(_couchbase(cluster_name, num), uid)
    return '{0}:{1}'.format(hostname, CLIENT_PROXY_PORT)


def _node_up(command, cluster_name, num, dns, image, uid, docker_host):
    publish = ALL_COUCHBASE_PORTS if docker_host else []

    hostname = common.format_hostname(_couchbase(cluster_name, num), uid)
    node = docker.run(
        image=image,
        name=hostname,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        dns_list=dns,
        publish=publish,
        command=command,
        docker_host=docker_host)

    # Make sure the command succeeded - the output should be a docker id (in hex)
    try:
        int(node, 16)
    except:
        raise Exception('Cannot start couchbase: {}'.format(node))

    couchbase_node = docker_host['ssh_hostname'] if docker_host else hostname

    return {
        'docker_ids': [node],
        'couchbase_nodes': [couchbase_node]
    }


def _ready(container, docker_host=None):
    if docker_host:
        hostname = docker_host['ssh_hostname']
    else:
        hostname = docker.inspect(container)['NetworkSettings']['IPAddress']

    url = 'http://{0}:{1}/pools'.format(hostname, ADMIN_PORT)
    try:
        r = requests.head(url, timeout=REQUEST_TIMEOUT)
        return r.status_code == requests.codes.ok
    except requests.ConnectionError:
        return False


def _wait_until(condition, containers, docker_host=None):
    common.wait_until(condition, containers, COUCHBASE_READY_WAIT_SECONDS,
                      docker_host)


def _cluster_nodes(containers, cluster_name, master_hostname, uid,
                   docker_host=None):
    for num, container in enumerate(containers[1:]):
        hostname = common.format_hostname(_couchbase(cluster_name, num + 1),
                                          uid)
        assert 0 == docker.exec_(container, docker_host=docker_host,
                                 command=["/opt/couchbase/bin/couchbase-cli",
                                          "server-add", "-c",
                                          "{0}:{1}".format(master_hostname,
                                                           ADMIN_PORT),
                                          "-u", "admin", "-p", "password",
                                          "--server-add={0}:{1}".format(
                                              hostname, port),
                                          "--server-add-username=admin",
                                          "--server-add-password=password"],
                                 stdout=sys.stderr)


def up(image, dns, uid, cluster_name, nodes, buckets={'onedata': 512},
       cluster_ramsize=1024, docker_host=None):
    if docker_host:
        print('Starting couchbase on remote host: {0}@{1}:{2}'.format(
            docker_host['ssh_username'],
            docker_host['ssh_hostname'],
            docker_host['ssh_port'] if 'ssh_port' in docker_host else 22
        ))

    dns_servers, dns_output = dns_mod.maybe_start(dns, uid)
    couchbase_output = {}

    command = '''/etc/init.d/couchbase-server start
bash'''

    for num in range(nodes):
        node_out = _node_up(command, cluster_name, num, dns_servers, image, uid,
                            docker_host)
        common.merge(couchbase_output, node_out)

    containers = couchbase_output['docker_ids']
    common.merge(couchbase_output, dns_output)

    _wait_until(_ready, containers, docker_host)

    master_hostname = common.format_hostname(_couchbase(cluster_name, 0), uid)

    # Initialize database cluster
    assert 0 == docker.exec_(containers[0], docker_host=docker_host,
                             command=["/opt/couchbase/bin/couchbase-cli",
                                      "cluster-init", "-c",
                                      "{0}:{1}".format(master_hostname,
                                                       ADMIN_PORT),
                                      "--cluster-init-username=admin",
                                      "--cluster-init-password=password",
                                      "--cluster-init-ramsize=" + str(
                                          cluster_ramsize)],
                             stdout=sys.stderr)

    # Create buckets
    for bucket_name, bucket_size in buckets.items():
        assert 0 == docker.exec_(containers[0], docker_host=docker_host,
                                 command=["/opt/couchbase/bin/couchbase-cli",
                                          "bucket-create", "-c",
                                          "{0}:{1}".format(master_hostname,
                                                           ADMIN_PORT),
                                          "-u", "admin", "-p", "password",
                                          "--bucket=" + bucket_name,
                                          "--bucket-ramsize=" + str(
                                              bucket_size),
                                          "--bucket-eviction-policy=fullEviction",
                                          "--wait"],
                                 stdout=sys.stderr)

    # Create database cluster nodes
    if len(containers) > 1:
        _cluster_nodes(containers, cluster_name, master_hostname, uid,
                       docker_host)

    # Rebalance all added nodes
    assert 0 == docker.exec_(containers[0], docker_host=docker_host,
                             command=["/opt/couchbase/bin/couchbase-cli",
                                      "rebalance", "-c",
                                      "{0}:{1}".format(master_hostname,
                                                       ADMIN_PORT),
                                      "-u", "admin", "-p", "password"],
                             stdout=sys.stderr)

    common.merge(couchbase_output, dns_output)

    return couchbase_output
