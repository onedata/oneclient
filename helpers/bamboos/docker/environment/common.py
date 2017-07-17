# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A custom utils library used across docker scripts.
"""

from __future__ import print_function
import argparse
import inspect
import json
import os
import requests
import time
import sys
from . import docker
from timeouts import *
import tempfile
import stat

try:
    import xml.etree.cElementTree as eTree
except ImportError:
    import xml.etree.ElementTree as eTree

requests.packages.urllib3.disable_warnings()

HOST_STORAGE_PATH = "/tmp/onedata"


def nagios_up(ip, port=None, protocol='https'):
    url = '{0}://{1}{2}/nagios'.format(protocol, ip, (':' + port) if port else '')
    try:
        r = requests.get(url, verify=False, timeout=REQUEST_TIMEOUT)
        if r.status_code != requests.codes.ok:
            return False

        healthdata = eTree.fromstring(r.text)
        return healthdata.attrib['status'] == 'ok'
    except requests.exceptions.RequestException:
        return False


def wait_until(condition, containers, timeout):
    deadline = time.time() + timeout
    for container in containers:
        while not condition(container):
            if time.time() > deadline:
                message = 'Timeout while waiting for condition {0} ' \
                          'of container {1}'
                message = message.format(condition.__name__, container)
                raise ValueError(message)

            time.sleep(1)


def standard_arg_parser(desc):
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description=desc)

    parser.add_argument(
        '-i-', '--image',
        action='store',
        default='onedata/worker',
        help='docker image to use for the container',
        dest='image')

    parser.add_argument(
        '-b', '--bin',
        action='store',
        default=os.getcwd(),
        help='path to the code repository (precompiled)',
        dest='bin')

    parser.add_argument(
        '-d', '--dns',
        action='store',
        default='auto',
        help='IP address of DNS or "none" - if no dns should be started or \
             "auto" - if it should be started automatically',
        dest='dns')

    parser.add_argument(
        '-u', '--uid',
        action='store',
        default=generate_uid(),
        help='uid that will be concatenated to docker names',
        dest='uid')

    parser.add_argument(
        'config_path',
        action='store',
        help='path to json configuration file')

    return parser


def merge(d, merged):
    """Merge the dict merged into dict d by adding their values on
    common keys
    """
    for key, value in iter(merged.items()):
        if key in d:
            if isinstance(value, dict):
                merge(d[key], value)
            else:
                d[key] = d[key] + value
        else:
            d[key] = value


def get_file_dir(file_path):
    """Returns the absolute path to directory containing given file"""
    return os.path.dirname(os.path.realpath(file_path))


def get_script_dir():
    """Returns the absolute path to directory containing the caller script"""
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    return get_file_dir(caller_mod.__file__)


def parse_json_config_file(path):
    """Parses a JSON file and returns a dict."""
    with open(path, 'r') as f:
        config = json.load(f)
        fix_sys_config_walk(config, None, [], path)
        return config


def fix_sys_config_walk(element, current_app_name, parents, file_path):
    app_names = apps_with_sysconfig()

    if isinstance(element, dict):
        for key, next_element in element.items():
            parents_of_next = list(parents)
            parents_of_next.append(key)

            if key in app_names:
                fix_sys_config_walk(next_element, key, parents_of_next, file_path)
            elif key == "sys.config":
                if current_app_name not in next_element:
                    element["sys.config"] = {current_app_name: next_element}
                    sys.stderr.write('''WARNING:
    Detected deprecated sys.config syntax
    Update entry to: 'sys.config': {'%s': {\*your config*\}}
    See entry at path: %s
    In file %s
''' % (current_app_name, ": ".join(parents), file_path))
            else:
                fix_sys_config_walk(next_element, current_app_name, parents_of_next, file_path)
    elif isinstance(element, list):
        for next_element in element:
            fix_sys_config_walk(next_element, current_app_name, parents, file_path)


def apps_with_sysconfig():
    return ["cluster_manager", "appmock", "cluster_worker", "op_worker",
            "globalregistry", "onepanel", "oneclient", "oz_worker"]


def get_docker_name(name_or_container):
    config = docker.inspect(name_or_container)
    return config['Name'].lstrip('/')


def get_docker_ip(name_or_container):
    config = docker.inspect(name_or_container)
    return config['NetworkSettings']['IPAddress']


def env_domain_name():
    """Returns domain name used in the environment. It will be concatenated
    to the dockernames (=hostnames) of all dockers.
    """
    return 'dev'


def format_hostname(domain_parts, uid):
    """Formats hostname for a docker based on domain parts and uid.
    NOTE: Hostnames are also used as docker names!
    domain_parts - a single or a list of consecutive domain parts that constitute a unique name
    within environment e.g.: ['worker1', 'prov1'], ['cm1', 'prov1'], 'client1'
    uid - timestamp
    """
    if isinstance(domain_parts, (str, unicode)):
        domain_parts = [domain_parts]
    domain_parts.extend([uid, env_domain_name()])
    # Throw away any '@' signs - they were used some time ago in node names
    # and might still occur by mistake.
    return '.'.join(domain_parts).replace('@', '')


def format_erl_node_name(app_name, hostname):
    """Formats full node name for an erlang VM hosted on docker based on app_name and hostname.
    NOTE: Hostnames are also used as docker names!
    app_name - application name, e.g.: 'cluster_manager', 'oz_worker'
    hostname - hostname aquired by format_*_hostname
    """
    return '{0}@{1}'.format(app_name, hostname)


def generate_uid():
    """Returns a uid (based on current time),
    that can be used to group dockers in DNS
    """
    return str(int(time.time()))


def create_users(container, users):
    """Creates system users on docker specified by 'container'.
    """
    for user in users:
        uid = str(hash(user) % 50000 + 10000)
        command = ["adduser", "--disabled-password", "--gecos", "''",
                   "--uid", uid, user]
        assert 0 is docker.exec_(container, command, interactive=True)


def create_groups(container, groups):
    """Creates system groups on docker specified by 'container'.
    """
    for group in groups:
        gid = str(hash(group) % 50000 + 10000)
        command = ["groupadd", "-g", gid, group]
        assert 0 is docker.exec_(container, command, interactive=True)
        for user in groups[group]:
            command = ["usermod", "-a", "-G", group, user]
            assert 0 is docker.exec_(container, command, interactive=True)


def volume_for_storage(storage, readonly=False):
    """Returns tuple (path_on_host, path_on_docker, read_write_mode)
    for a given storage
    """
    return storage_host_path(), storage, 'ro' if readonly else 'rw'


def storage_host_path():
    """Returns path to temporary directory for storage on host
    """
    if not os.path.exists(HOST_STORAGE_PATH):
        os.makedirs(HOST_STORAGE_PATH)
    tmpdir = tempfile.mkdtemp(dir=HOST_STORAGE_PATH)
    os.chmod(tmpdir,  stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)
    return tmpdir


def mount_nfs_command(config, storages_dockers):
    """Prepares nfs mount commands for specified os_config and storage dockers
    :param config: config that may contain os_config inside
    :param storages_dockers: storage dockers map
    :return: string with commands
    """
    mount_command = ''
    if not storages_dockers:
        return mount_command
    if 'os_config' in config:
        for storage in config['os_config']['storages']:
            if storage['type'] == 'nfs':
                mount_command += '''
mkdir -p {mount_point}
mount -t nfs -o proto=tcp,port=2049,nolock {host}:/exports {mount_point}
'''.format(host=storages_dockers['nfs'][storage['name']]['ip'], mount_point=storage['name'])
    return mount_command

