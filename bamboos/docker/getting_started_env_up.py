#!/usr/bin/env python
# coding=utf-8

"""Author: Michał Ćwiertnia
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

This file is mainly used in onedata tests.

Starts scenario 2.0 from onedata's getting started.
Runs isolated Onedata deployment consisting of:
- a single node preconfigured Onezone instance
- a single node preconfigured Oneprovider instance

To run this script manually:
- run script from onedata repo root dir
- make sure there is tests/gui directory in onedata repo root dir
- make sure you have python libraries: urllib3, certifi
- make sure you have getting_started, onezone_swagger and onepanel_swagger submodules
- build swagger clients running command: "make build_swaggers" from onedata repo root dir

Run the script with -h flag to learn about script's running options.
"""

# onepanel_client is generated after running onedata's makefile with command make swaggers
import sys
import traceback
sys.path.append('.')

try:
    from tests.gui.onepanel_client import ApiClient as ApiClient_OP
    from tests.gui.onepanel_client import OnepanelApi
    from tests.gui.onepanel_client.configuration import Configuration as Conf_Onepanel
    from tests.gui.onepanel_client import OneproviderApi
    from tests.gui.onepanel_client import ProviderModifyRequest
except ImportError:
    print 'Cannot import swagger clients. You have to generate swagger clients using ' \
          '\'make swaggers\' command in onedata directory'
    traceback.print_exc()
    exit(1)

from environment import docker
from subprocess import Popen, PIPE, STDOUT, call
import re
import time
import argparse
import os
import json


scenario_2_0_path = os.path.join('getting_started', 'scenarios', '2_0_oneprovider_onezone')
scenario_2_0_network = '20oneprovideronezone_scenario2'
timeout = 60 * 10
start_onezone_args = ['--zone', '--detach', '--with-clean']
start_oneprovider_args = ['--provider', '--detach', '--without-clean']
PANEL_REST_PORT = 9443
PANEL_REST_PATH_PREFIX = "/api/v3/onepanel"


def print_logs(service_name, service_docker_logs):
    print '{} docker logs '.format(service_name)
    print service_docker_logs

    path = os.path.join(scenario_2_0_path, 'config_' + service_name, 'var', 'log')
    try:
        directories = os.listdir(path)
    except IOError:
        print 'Couldn\'t find {}'.format(path)
        return

    for dir in directories:
        try:
            files = os.listdir(os.path.join(path, dir))
        except IOError:
            print 'Couldn\'t find {}'.format(os.path.join(path, dir))
            break
        for file in files:
            try:
                with open(os.path.join(path, dir, file), 'r') as logs:
                    print '{service_name} {dir} {file}'.format(service_name=service_name,
                                                               dir=dir,
                                                               file=file)
                    print logs.readlines()
            except IOError:
                print 'Couldn\'t find {}'.format(os.path.join(path, dir, file))


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


def start_service(start_service_path, start_service_args, service_name, timeout):
    """
    service_name argument is one of: onezone, oneprovider
    Runs ./run_onedata.sh script from onedata's getting started scenario 2.0
    Returns ip of started service
    """
    service_process = Popen(['./run_onedata.sh'] + start_service_args, stdout=PIPE, stderr=STDOUT,
                            cwd=start_service_path)
    service_process.wait()
    service_output = service_process.communicate()[0]
    print service_output
    splited_service_output = service_output.split('\n')
    docker_name = [item for item in splited_service_output if re.search('Creating ' + service_name, item)]
    docker_name = docker_name[0].split()
    docker_name = docker_name[len(docker_name) - 1]

    # Wait for client to start
    service_docker_logs = ''
    check_if_service_is_up = ['docker', 'logs', docker_name]
    timeout = time.time() + timeout
    while not (re.search('Congratulations', service_docker_logs)):
        service_process = Popen(check_if_service_is_up, stdout=PIPE, stderr=STDOUT)
        service_process.wait()
        service_docker_logs = service_process.communicate()[0]
        if re.search('Error', service_docker_logs):
            print 'Error while starting {}'.format(service_name)
            print_logs(service_name, service_docker_logs)
            exit(1)
        if time.time() > timeout:
            print 'Timeout while starting {}'.format(service_name)
            print_logs(service_name, service_docker_logs)
            exit(1)
        time.sleep(2)
    print '{} has started'.format(service_name)

    # Get ip of service
    service_docker_logs = service_docker_logs.split('\n')
    service_ip = [item for item in service_docker_logs if re.search('IP Address', item)]
    if len(service_ip) == 0:
        print 'Couldn\'t find {} IP address'.format(service_name)
        print_logs(service_name, service_docker_logs)
        exit(1)
    service_ip = service_ip[0].split()
    service_ip = service_ip[len(service_ip) - 1]
    print '{service_name} IP: {service_ip}'.format(service_name=service_name,
                                                   service_ip=service_ip)
    return service_ip


parser = argparse.ArgumentParser()
parser.add_argument('--admin-credentials', action='store', default='admin:password',
                    help='Username and password for admin user', required=False)
parser.add_argument('--docker-name', action='store', default='',
                    help='Name of docker that will be added to network', required=False)
args = parser.parse_args()

print 'Starting onezone'
rm_persistence(scenario_2_0_path, 'onezone')
onezone_ip = start_service(scenario_2_0_path, start_onezone_args, 'onezone', timeout)
print 'Starting oneprovider'
rm_persistence(scenario_2_0_path, 'oneprovider')
oneprovider_ip = start_service(scenario_2_0_path, start_oneprovider_args, 'oneprovider', timeout)

if args.docker_name:
    docker.connect_docker_to_network(scenario_2_0_network, args.docker_name)

# Configure environment
print 'Configuring environment'
onezone_address = 'https://{}'.format(onezone_ip)
oneprovider_address = 'https://{}'.format(oneprovider_ip)

# Create actual REST API endpoint for onepanel
oz_panel_REST_ENDPOINT = '{address}:{REST_PORT}' \
                         '{REST_PATH_PREFIX}'.format(address=onezone_address,
                                                     REST_PORT=PANEL_REST_PORT,
                                                     REST_PATH_PREFIX=PANEL_REST_PATH_PREFIX)
op_panel_REST_ENDPOINT = '{address}:{REST_PORT}' \
                         '{REST_PATH_PREFIX}'.format(address=oneprovider_address,
                                                     REST_PORT=PANEL_REST_PORT,
                                                     REST_PATH_PREFIX=PANEL_REST_PATH_PREFIX)

# Only necessary when connecting to a private Onezone instance
Conf_Onepanel().verify_ssl = False

# Set Configuration in Onepanel for admin 'admin'
username, password = args.admin_credentials.split(':')
Conf_Onepanel().username = username
Conf_Onepanel().password = password

# Login as admin 'admin' to oneprovider panel
op_panel_client = ApiClient_OP(host=op_panel_REST_ENDPOINT,
                               header_name='authorization',
                               header_value=Conf_Onepanel().get_basic_auth_token())

# Create oneprovider api for admin 'admin'
oneprovider_api = OneproviderApi(op_panel_client)

# Change provider redirection point and name
provider_modify_request = ProviderModifyRequest(redirection_point=oneprovider_address)
oneprovider_api.patch_provider(provider_modify_request)

output = {
    'oneprovider_host': oneprovider_ip,
    'onezone_host': onezone_ip,
    'op_panel_host': oneprovider_ip,
    'oz_panel_host': onezone_ip
}

print json.dumps(output)
