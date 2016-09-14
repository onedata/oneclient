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


def _couchbase(cluster_name, num):
    return 'couchbase{0}-{1}'.format(num, cluster_name)


def config_entry(cluster_name, num, uid):
    return '{0}:11211'.format(common.format_hostname(_couchbase(cluster_name, num), uid))


def _node_up(command, cluster_name, num, dns, image, uid):
    hostname = common.format_hostname(_couchbase(cluster_name, num), uid)
    node = docker.run(
        image=image,
        name=hostname,
        hostname=hostname,
        detach=True,
        interactive=True,
        tty=True,
        dns_list=dns,
        command=command)

    return {
        'docker_ids': [node],
        'couchbase_nodes': [hostname]
    }


def _ready(container):
    ip = docker.inspect(container)['NetworkSettings']['IPAddress']
    url = 'http://{0}:8091/pools'.format(ip)
    try:
        r = requests.head(url, timeout=REQUEST_TIMEOUT)
        return r.status_code == requests.codes.ok
    except requests.ConnectionError:
        return False


def _wait_until(condition, containers):
    common.wait_until(condition, containers, COUCHBASE_READY_WAIT_SECONDS)


def _cluster_nodes(containers, cluster_name, master_hostname, uid):
    for num, container in enumerate(containers[1:]):
        hostname = common.format_hostname(_couchbase(cluster_name, num + 1), uid)
        assert 0 == docker.exec_(container,
                     command=["/opt/couchbase/bin/couchbase-cli", "server-add", "-c", "{0}:8091".format(master_hostname),
                              "-u", "admin", "-p", "password", "--server-add={0}:8091".format(hostname),
                              "--server-add-username=admin", "--server-add-password=password", "--services=data,index,query"],
                 stdout=sys.stderr)


def up(image, dns, uid, cluster_name, nodes, buckets={'default':512}, cluster_ramsize=1024):

    dns_servers, dns_output = dns_mod.maybe_start(dns, uid)
    couchbase_output = {}

    command = '''/etc/init.d/couchbase-server start
bash'''

    for num in range(nodes):
        node_out = _node_up(command, cluster_name, num, dns_servers, image, uid)
        common.merge(couchbase_output, node_out)

    containers = couchbase_output['docker_ids']
    common.merge(couchbase_output, dns_output)

    _wait_until(_ready, containers)

    master_hostname = common.format_hostname(_couchbase(cluster_name, 0), uid)

    # Initialize database cluster
    assert 0 == docker.exec_(containers[0],
                 command=["/opt/couchbase/bin/couchbase-cli", "cluster-init", "-c", "{0}:8091".format(master_hostname),
                          "--cluster-init-username=admin", "--cluster-init-password=password",
                          "--cluster-init-ramsize=" + str(cluster_ramsize), "--services=data,index,query"],
                 stdout=sys.stderr)

    # Create buckets
    for bucket_name, bucket_size in buckets.items():
        assert 0 == docker.exec_(containers[0],
                 command=["/opt/couchbase/bin/couchbase-cli", "bucket-create", "-c", "{0}:8091".format(master_hostname),
                          "-u", "admin", "-p", "password", "--bucket=" + bucket_name,
                          "--bucket-ramsize=" + str(bucket_size), "--wait"],
                 stdout=sys.stderr)

    # Create database cluster nodes
    if len(containers) > 1:
        _cluster_nodes(containers, cluster_name, master_hostname, uid)

    # Rebalance all added nodes
    assert 0 == docker.exec_(containers[0],
                 command=["/opt/couchbase/bin/couchbase-cli", "rebalance", "-c", "{0}:8091".format(master_hostname),
                          "-u", "admin", "-p", "password"],
                 stdout=sys.stderr)

    common.merge(couchbase_output, dns_output)

    return couchbase_output
