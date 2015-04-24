# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a DNS server with container (skydns + skydock) that allow
different dockers to see each other by hostnames.
"""

from __future__ import print_function

from . import common, docker


DNS_WAIT_FOR_SECONDS = 60


def _skydns_ready(container):
    return 'Initializing new cluster' in docker.logs(container)


def _skydock_ready(container):
    return 'skydock: starting main process' in docker.logs(container)


def set_up_dns(config, uid):
    """Sets up DNS configuration values, starting the server if needed."""
    if config == 'auto':
        dns_config = up(uid)
        return [dns_config['dns']], dns_config

    if config == 'none':
        return [], {}

    return [config], {}


def up(uid):
    create_service = '{0}/createService.js'.format(common.get_script_dir())

    skydns = docker.run(
        image='crosbymichael/skydns',
        detach=True,
        name=common.format_dockername('skydns', uid),
        command=['-nameserver', '8.8.8.8:53', '-domain', 'docker'])

    common.wait_until(_skydns_ready, [skydns], DNS_WAIT_FOR_SECONDS)

    skydock = docker.run(
        image='crosbymichael/skydock',
        detach=True,
        name=common.format_dockername('skydock', uid),
        reflect=[('/var/run/docker.sock', 'rw')],
        volumes=[(create_service, '/createService.js', 'ro')],
        command=['-ttl', '30', '-environment', 'dev', '-s',
                 '/var/run/docker.sock',
                 '-domain', 'docker', '-name', 'skydns_{0}'.format(uid),
                 '-plugins',
                 '/createService.js'])

    common.wait_until(_skydock_ready, [skydock], DNS_WAIT_FOR_SECONDS)

    skydns_config = docker.inspect(skydns)
    dns = skydns_config['NetworkSettings']['IPAddress']

    return {'dns': dns, 'docker_ids': [skydns, skydock]}
