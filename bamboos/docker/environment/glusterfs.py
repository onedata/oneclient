# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2017 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a GlusterFS storage.
"""

import re
import sys
from timeouts import *

from . import common, docker

def _glusterfs_ready(container):
    output = docker.exec_(container, ['bash', '-c', 'gluster peer status || true'],
                          output=True, stdout=sys.stderr)
    return bool(re.search('Number of Peers', output))

def _node_up(image, volumes, name, uid, transport, mountpoint):
    hostname = common.format_hostname([name, 'glusterfs'], uid)

    container = docker.run(
        image=image,
        privileged=True,
        hostname=hostname,
        name=hostname,
        volumes=[],
        detach=True)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']
    port = 24007

    common.wait_until(_glusterfs_ready, [container], GLUSTERFS_READY_WAIT_SECONDS)

    for volume in volumes:
        # Create GlusterFS volume inside container
        output = docker.exec_(container, [
            'bash', '-c',
            "gluster volume create %s %s:/mnt/%s/ force"%(volume, ip, volume)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "gluster volume set %s config.transport %s"%(volume, transport)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "gluster volume set %s nfs.ports-insecure on"%(volume)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "gluster volume set %s server.allow-insecure on"%(volume)],
                output=True, stdout=sys.stderr)

        # Start GlusterFS volume
        output = docker.exec_(container, [
            'bash', '-c',
            "gluster volume start %s"%(volume)],
                output=True, stdout=sys.stderr)

        # Create mountpoint directory inside the volume
        output = docker.exec_(container, [
            'bash', '-c',
            "mkdir /mnt/%s-tmp"%(volume)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "mount -t glusterfs %s:%s /mnt/%s-tmp"%(ip, volume, volume)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "mkdir -p /mnt/%s-tmp/%s"%(volume, mountpoint)],
                output=True, stdout=sys.stderr)
        output = docker.exec_(container, [
            'bash', '-c',
            "umount /mnt/%s-tmp"%(volume)],
                output=True, stdout=sys.stderr)

    return {
        'docker_ids': [container],
        'host_name': ip,
        'port': port,
        'transport': transport,
        'mountpoint': mountpoint
    }


def up(image, volumes, name, uid, transport, mountpoint):
    return _node_up(image, volumes, name, uid, transport, mountpoint)
