"""A custom utils library used across docker scripts."""

import inspect
import json
import os
import subprocess
import time


def merge(d, merged):
    """Merge the dict merged into dict d by adding their values on
    common keys
    """
    for key, value in iter(merged.items()):
        d[key] = d[key] + value if key in d else value


def set_up_dns(config, uid):
    """Sets up DNS configuration values, starting the server if needed."""
    if config == 'auto':
        dns_config = run_script_return_dict('dns_up.py', ['--uid', uid])
        return ([dns_config['dns']], dns_config)

    if config == 'none':
        return ([], {})
    
    return ([config], {})


def get_file_dir(file_path):
    """Returns the absolute path to directory containing given file"""
    return os.path.dirname(os.path.realpath(file_path))


def get_script_dir():
    """Returns the absolute path to directory containing the caller script"""
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    return get_file_dir(caller_mod.__file__)


def run_script_return_dict(script, args):
    """Runs given script that resides in the same directory and returns JSON.
    Parses the JSON and returns a dict.
    """
    cmd = [get_file_dir(__file__) + '/' + script] + args
    result = subprocess.Popen(cmd,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE,
                              stdin=subprocess.PIPE).communicate()[0]
    return json.loads(result.decode('utf-8'))


def parse_json_file(path):
    """Parses a JSON file and returns a dict."""
    with open(path, 'r') as f:
        return json.load(f)


def format_hostname(node_name, uid):
    """Formats hostname for a docker based on node name and uid
    node_name can be in format 'somename@' or 'somename'.
    This is needed so different components are resolvable through DNS.
    """
    (name, _, _) = node_name.partition('@')
    return '{0}@{0}.{1}.dev.docker'.format(name, uid)


def format_dockername(node_name, uid):
    """Formats docker name based on node name and uid
    node_name can be in format 'somename@' or 'somename'.
    This is needed so different components are resolvable through DNS.
    """
    (name, _, _) = node_name.partition('@')
    return '{0}_{1}'.format(name, uid)


def generate_uid():
    """Returns a uid (based on current time),
    that can be used to group dockers in DNS
    """
    return str(int(time.time()))
