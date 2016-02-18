# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a Ceph storage cluster.
"""

import re
import sys

from . import common, docker

CEPH_READY_WAIT_SECONDS = 60 * 5


def _ceph_ready(container):
    output = docker.exec_(container, ['ceph', 'health'], output=True,
                          stdout=sys.stderr)
    return bool(re.search('HEALTH_OK', output))


def _node_up(image, pools):
    container = docker.run(
            image=image,
            run_params=["--privileged"],
            detach=True)

    for (name, pg_num) in pools:
        docker.exec_(container, ['ceph', 'osd', 'pool', 'create', name, pg_num])

    common.wait_until(_ceph_ready, [container], CEPH_READY_WAIT_SECONDS)

    username = 'client.admin'
    key = docker.exec_(container, ['ceph', 'auth', 'print-key', username],
                       output=True)

    return {
        'docker_ids': [container],
        'username': username,
        'key': key
    }


def up(image, pools):
    return _node_up(image, pools)
