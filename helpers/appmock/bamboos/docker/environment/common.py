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

try:
    import xml.etree.cElementTree as eTree
except ImportError:
    import xml.etree.ElementTree as eTree

requests.packages.urllib3.disable_warnings()


def nagios_up(ip, port=None):
    url = 'https://{0}{1}/nagios'.format(ip, (':' + port) if port else '')
    try:
        r = requests.get(url, verify=False, timeout=5)
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


def parse_json_file(path):
    """Parses a JSON file and returns a dict."""
    with open(path, 'r') as f:
        return json.load(f)


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
    within environment e.g.: ['worker1', 'prov1'], ['ccm1', 'prov1'], 'client1'
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
    app_name - application name, e.g.: 'op_ccm', 'globalregistry'
    hostname - hostname aquired by format_*_hostname
    """
    return '{0}@{1}'.format(app_name, hostname)


def generate_uid():
    """Returns a uid (based on current time),
    that can be used to group dockers in DNS
    """
    return str(int(time.time()))
