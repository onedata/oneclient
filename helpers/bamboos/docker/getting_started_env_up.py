#!/usr/bin/env python
# coding=utf-8

"""Author: Michał Ćwiertnia
Copyright (C) 2016-2017 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

This file is mainly used in onedata tests.

Starts scenario 2.0 or 2.1 from onedata's getting started.
Runs isolated Onedata deployment consisting of:
- a single node Onezone instance
- a single node Oneprovider instances

To run this script manually:
- run script from onedata repo root dir
- make sure there is tests/gui directory in onedata repo root dir
- make sure you have python libraries: urllib3, certifi
- make sure you have getting_started, onezone_swagger and onepanel_swagger submodules
- build swagger clients running command: "make build_swaggers" from onedata repo root dir

Run the script with -h flag to learn about script's running options.
"""

import sys

sys.path.append('.')

from environment import docker
from environment.common import ensure_provider_oz_connectivity
from subprocess import Popen, PIPE, STDOUT, call
import os
import re
import time
import json
import argparse

PROVIDER_DOCKER_COMPOSE_FILE = 'docker-compose-oneprovider.yml'
ZONE_DOCKER_COMPOSE_FILE = 'docker-compose-onezone.yml'
SCENARIOS_DIR_PATH = os.path.join('getting_started', 'scenarios')
SERVICE_LOGS_DIR = '/volumes/persistence/var/log'
TIMEOUT = 60 * 10


def print_logs(service_name, service_docker_logs):
    print '{} docker logs:'.format(service_name)
    print service_docker_logs

    path = os.path.join(scenario_path, 'config_' + service_name, 'var', 'log')
    try:
        directories = os.listdir(path)
    except IOError:
        print 'Couldn\'t find {}'.format(path)
    else:
        for directory in directories:
            try:
                files = os.listdir(os.path.join(path, directory))
            except IOError:
                print 'Couldn\'t find {}'.format(os.path.join(path, directory))
            else:
                for file in files:
                    try:
                        with open(os.path.join(path, directory, file), 'r') \
                                as logs:
                            print '{service_name} {dir} {file}'.format(
                                service_name=service_name,
                                dir=directory,
                                file=file)

                            print logs.readlines()
                    except IOError:
                        print 'Couldn\'t find {}'.format(
                            os.path.join(path, directory, file))


PERSISTENCE = ('# configuration persistance',
               '# data persistance',
               '# configuration persistence',
               '# data persistence')


def rm_persistence(path, service_name):
    """Remove persistence of configuration/data.
    """
    service_path = os.path.join(path, 'docker-compose-{}.yml'
                                      ''.format(service_name))
    with open(service_path, 'r+') as f:
        lines = f.readlines()

        comment = False
        for i, line in enumerate(lines):
            if comment:
                lines[i] = '#{}'.format(line) if line[0] != '#' else line
                comment = False
            if any(per in line for per in PERSISTENCE):
                comment = True

        f.seek(0)
        f.writelines(lines)


def add_etc_hosts_entries(service_ip, service_host):
    call('sudo bash -c "echo {} {} >> /etc/hosts"'.format(service_ip, 
                                                              service_host),
                                                              shell=True)


def wait_for_service_start(service_name, docker_name, pattern, timeout):
    timeout = time.time() + timeout
    docker_logs = ''
    while not re.search(pattern, docker_logs, re.I):
        service_process = Popen(['docker', 'logs', docker_name], stdout=PIPE,
                                stderr=STDOUT)
        docker_logs = service_process.communicate()[0]
        if re.search('Error', docker_logs):
            print 'Error while starting {}'.format(service_name)
            print_logs(service_name, docker_logs)
            exit(1)
        if time.time() > timeout:
            print 'Timeout while starting {}\'s'.format(service_name)
            print_logs(service_name, docker_logs)
            exit(1)
        time.sleep(2)


def start_service(start_service_path, start_service_args, service_name,
                  timeout, etc_hosts_entries):
    """
    service_name argument is either 'onezone' or provider name
    Runs ./run_onedata.sh script from given onedata's getting started scenario
    Returns hostname and docker name of started service
    """
    service_process = Popen(['./run_onedata.sh'] + start_service_args,
                            stdout=PIPE, stderr=STDOUT, cwd=start_service_path)
    service_output = service_process.communicate()[0]

    print service_output

    docker_name = re.search(r'Creating\s*(?P<name>{})\b'.format(
        service_name), service_output).group('name')

    if '2_1' in args.scenario:
        wait_for_service_start(service_name, docker_name,
                               r'IP Address:\s*(\d{1,3}\.?){4}',
                               timeout)
    else:
        wait_for_service_start(service_name, docker_name,
                               r'.*congratulations.*successfully.*started.*',
                               timeout)

    # get service ip and hostname
    format_options = ('\'{{with index .NetworkSettings.Networks "' +
                      scenario_network + '"}}{{.IPAddress}}{{end}} '
                                         '{{ .Config.Hostname }} {{ .Config.Domainname }}\'')

    service_process = Popen(['docker', 'inspect',
                             '--format={}'.format(format_options), docker_name],
                            stdout=PIPE, stderr=STDOUT)
    docker_conf = service_process.communicate()[0].strip().strip("\'")
    ip, docker_hostname, docker_domain = docker_conf.split()
    service_host = '{}.{}'.format(docker_hostname, docker_domain)

    if service_host.endswith('.'):
        service_host = service_host[:-1]

    if args.write_to_etc_hosts:
        add_etc_hosts_entries(ip, service_host)
        add_etc_hosts_entries(ip, '{}.{}'.format(service_host,'test'))
    else:
        etc_hosts_entries[ip] = service_host

    return service_host, docker_name


def set_logs_dir(base_file_str, logs_dir, dockerfile, alias):
    file_str = re.sub(r'(volumes:)(\s*)',
                      r'\1\2- "{}:{}"\2'.format(
                          os.path.join(logs_dir, alias), SERVICE_LOGS_DIR),
                      base_file_str)

    with open(dockerfile, 'w') as f:
        f.write(file_str)


def modify_provider_docker_compose(provider_name, base_file_str,
                                   provider_dockerfile):
    file_str = re.sub(r'(services:\s*).*', r'\1node1.{}.local:'.format(provider_name),
                      base_file_str)
    file_str = re.sub(r'(image:.*\s*hostname: ).*',
                      r'\1node1.{}.local'.format(provider_name),
                      file_str)
    file_str = re.sub(r'container_name: .*',
                      r'container_name: {}'.format(provider_name), file_str)
    file_str = re.sub(r'(cluster:\s*domainName: ).*',
                      r'\1"{}.local"'.format(provider), file_str)
    file_str = re.sub(r'domain: .*',
                      r'domain: "node1.{}.local"'.format(
                          provider_name), file_str)
    if args.logs_dir:
        set_logs_dir(file_str, os.path.join(os.getcwd(), args.logs_dir),
                     provider_dockerfile, provider_name)
    else:
        with open(provider_dockerfile, 'w') as f:
            f.write(file_str)

parser = argparse.ArgumentParser()
parser.add_argument('--docker-name',
                    action='store',
                    default='',
                    help='Name of docker that will be added to network',
                    required=False)
parser.add_argument('--scenario',
                    action='store',
                    default='2_0_oneprovider_onezone',
                    help='Getting started scenario\'s name',
                    required=False)
parser.add_argument('--zone-name',
                    action='store',
                    default='z1',
                    dest='zone_name',
                    help='Zone\'s name',
                    required=False)
parser.add_argument('--providers-names',
                    nargs='+',
                    default=['p1'],
                    dest='providers_names',
                    help='List of providers names separated with space',
                    required=False)
parser.add_argument('--write-to-etc-hosts',
                    action='store_true',
                    dest='write_to_etc_hosts',
                    help='If given write ip-hostname mappings to '
                         '/etc/hosts else ip-hostname mappings will'
                         'be printed to stdout',
                    required=False)
parser.add_argument('--l', '--logs_dir',
                    action='store',
                    default='',
                    dest='logs_dir',
                    help='Directory where logs should be placed',
                    required=False)

args = parser.parse_args()

scenario_path = os.path.join(SCENARIOS_DIR_PATH, args.scenario)
scenario_network = '{}_{}'.format(args.scenario.replace('_', ''), 'scenario2')
etc_hosts_entries = {}


zone_dockerfile = os.path.join(SCENARIOS_DIR_PATH, args.scenario,
                               ZONE_DOCKER_COMPOSE_FILE)
print 'Starting onezone'
start_onezone_args = ['--zone', '--detach', '--with-clean', '--name',
                      args.zone_name]
rm_persistence(scenario_path, 'onezone')

if args.logs_dir:
    with open(zone_dockerfile) as f:
        base_zone_dockerfile = f.read()
    try:
        set_logs_dir(base_zone_dockerfile,
                     os.path.join(os.getcwd(), args.logs_dir), zone_dockerfile,
                     args.zone_name)
        oz_hostname, oz_docker_name = start_service(scenario_path,
                                                    start_onezone_args,
                                                    'onezone-1', TIMEOUT,
                                                    etc_hosts_entries)
    finally:
        with open(zone_dockerfile, "w") as f:
                f.write(base_zone_dockerfile)
else:
    oz_hostname, oz_docker_name = start_service(scenario_path,
                                                start_onezone_args,
                                                'onezone-1', TIMEOUT,
                                                etc_hosts_entries)

output = {
    'oz_worker_nodes': [
        {
            'alias': args.zone_name,
            'hostname': oz_hostname,
            'docker_name': oz_docker_name
        }
    ],
    'op_worker_nodes': [
    ]

}

provider_dockerfile = os.path.join(SCENARIOS_DIR_PATH, args.scenario,
                                   PROVIDER_DOCKER_COMPOSE_FILE)

with open(provider_dockerfile) as f:
    base_provider_dockerfile = f.read()

for provider in args.providers_names:
    print 'Starting provider: ' + provider
    start_oneprovider_args = ['--provider', '--detach', '--without-clean',
                              '--name', provider]
    try:
        modify_provider_docker_compose(provider, base_provider_dockerfile,
                                       provider_dockerfile)
        rm_persistence(scenario_path, 'oneprovider')
        op_hostname, op_docker_name = start_service(
            scenario_path, start_oneprovider_args, provider, TIMEOUT,
            etc_hosts_entries)

    finally:
        with open(provider_dockerfile, "w") as f:
            f.write(base_provider_dockerfile)

    output['op_worker_nodes'] += [
        {
            'alias': provider,
            'hostname': op_hostname,
            'docker_name': op_docker_name
        }
    ]

if args.docker_name:
    docker.connect_docker_to_network(scenario_network, args.docker_name)

# In scenario 2_0, OP and OZ are created and the provider is registered. In such
# case, make sure provider instances are connected to their zones before the
# test starts.
if '2_0' in args.scenario:
    print('Waiting for OZ connectivity of providers...')
    for op_node_output in output['op_worker_nodes']:
        if not ensure_provider_oz_connectivity(op_node_output['hostname']):
            raise Exception(
                'Could not ensure OZ connectivity of provider {0}'.format(
                    op_node_output['hostname']))
    print('OZ connectivity established')

for ip in etc_hosts_entries:
    print '{} {}'.format(ip, etc_hosts_entries[ip])

print json.dumps(output)
