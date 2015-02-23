"""Brings up a DNS server with container (skydns + skydock) that allow
different dockers to see each other by hostnames.
"""

import common
import docker


def up(uid):
    create_service = '{0}/createService.js'.format(common.get_script_dir())

    skydns = docker.run(
        image='crosbymichael/skydns',
        detach=True,
        name=common.format_dockername('skydns', uid),
        command=['-nameserver', '8.8.8.8:53', '-domain', 'docker'])

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

    skydns_config = docker.inspect(skydns)
    dns = skydns_config['NetworkSettings']['IPAddress']

    return {'dns': dns, 'docker_ids': [skydns, skydock]}
