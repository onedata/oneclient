#!/usr/bin/env python

"""
Brings up a DNS server with container (skydns + skydock) that allow
different dockers to see each other by hostnames.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import collections
import common
import docker
import json


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up skydns.')

parser.add_argument(
    '--create-service', '-c',
    action='store',
    default='{0}/createService.js'.format(common.get_script_dir()),
    help='path to createService.js plugin',
    dest='create_service')

parser.add_argument(
    '--uid', '-u',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker name',
    dest='uid')

args = parser.parse_args()
uid = args.uid

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
    volumes=[(args.create_service, '/createService.js', 'ro')],
    command=['-ttl', '30', '-environment', 'dev', '-s', '/var/run/docker.sock',
             '-domain', 'docker', '-name', 'skydns_{0}'.format(uid), '-plugins',
             '/createService.js'])

skydns_config = docker.inspect(skydns)
dns = skydns_config['NetworkSettings']['IPAddress']

output = {
    'dns': dns,
    'docker_ids': [skydns, skydock]
}

# Print JSON to output so it can be parsed by other scripts
print(json.dumps(output))
