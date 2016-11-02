# coding=utf-8
"""Author: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a Swift storage with Keystone auth.
"""

import sys
import time

from timeouts import *
from . import common, docker

SWIFT_COMMAND = 'swift --auth-version 2 -A http://{0}:5000/v2.0 -K swift ' \
                '-U swift --os-tenant-name service {1}'


def _get_swift_ready(ip):
    def _swift_ready(container):
        output = docker.exec_(container, [
            'bash', '-c', SWIFT_COMMAND.format(ip, 'list 2>&1')],
                              output=True, stdout=sys.stderr)
        return bool(output == '')

    return _swift_ready


def _node_up(image, containers, name, uid):
    hostname = common.format_hostname([name, 'swift'], uid)

    container = docker.run(
        image=image,
        hostname=hostname,
        name=hostname,
        privileged=True,
        detach=True,
        tty=True,
        interactive=True,
        envs={'INITIALIZE': 'yes'},
        run_params=["--entrypoint", "bash"])

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    docker.exec_(container,
                 ['bash', '-c',
                  'IPADDRESS={0} /sbin/my_init > /tmp/run.log'.format(ip)],
                 detach=True)

    common.wait_until(_get_swift_ready(ip), [container],
                      SWIFT_READY_WAIT_SECONDS)
    # Sometimes swift returns internal server error few times
    # after first successful request
    time.sleep(3)
    common.wait_until(_get_swift_ready(ip), [container],
                      SWIFT_READY_WAIT_SECONDS)

    for c in containers:
        assert '' == docker.exec_(container,
                                  SWIFT_COMMAND.format(ip, 'post ' + c).split(),
                                  output=True, stdout=sys.stderr)

    return {
        'docker_ids': [container],
        'user_name': 'swift',
        'password': 'swift',
        'tenant_name': 'service',
        'host_name': ip,
        'keystone_port': 5000,
        'swift_port': 8080,
        'keystone_admin_port': 35357,
        'swift_browser_port': 8000,
        'horizon_dashboard_port': 8081,
    }


def up(image, containers, name, uid):
    return _node_up(image, containers, name, uid)
