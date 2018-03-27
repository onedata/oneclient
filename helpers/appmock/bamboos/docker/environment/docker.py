# coding=utf-8
"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Functions wrapping capabilities of docker binary.
"""

import json
import os
import subprocess
import sys


# noinspection PyDefaultArgument
def run(image, docker_host=None, detach=False, dns_list=[], add_host={},
        envs={}, hostname=None, interactive=False, link={}, tty=False, rm=False,
        reflect=[], volumes=[], name=None, workdir=None, user=None, group=None,
        group_add=[], cpuset_cpus=None, privileged=False, publish=[], run_params=[], command=None,
        output=False, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    cmd.append('run')

    if detach:
        cmd.append('-d')

    for addr in dns_list:
        cmd.extend(['--dns', addr])

    for key, value in add_host.iteritems():
        cmd.extend(['--add-host', '{0}:{1}'.format(key, value)])

    for key in envs:
        cmd.extend(['-e', '{0}={1}'.format(key, envs[key])])

    if hostname:
        cmd.extend(['-h', hostname])

    if detach or sys.__stdin__.isatty():
        if interactive:
            cmd.append('-i')
        if tty:
            cmd.append('-t')

    for container, alias in link.items():
        cmd.extend(['--link', '{0}:{1}'.format(container, alias)])

    if name:
        cmd.extend(['--name', name])

    if rm:
        cmd.append('--rm')

    for path, read in reflect:
        vol = '{0}:{0}:{1}'.format(os.path.abspath(path), read)
        cmd.extend(['-v', vol])

    # Volume can be in one of three forms
    # 1. 'path_on_docker'
    # 2. ('path_on_host', 'path_on_docker', 'ro'/'rw')
    # 3. {'volumes_from': 'volume name'}
    for entry in volumes:
        if isinstance(entry, tuple):
            path, bind, readable = entry
            vol = '{0}:{1}:{2}'.format(os.path.abspath(path), bind, readable)
            cmd.extend(['-v', vol])
        elif isinstance(entry, dict):
            volume_name = entry['volumes_from']
            cmd.extend(['--volumes-from', volume_name])
        else:
            cmd.extend(['-v', entry])

    if workdir:
        cmd.extend(['-w', os.path.abspath(workdir)])

    if user:
        user_group = '{0}:{1}'.format(user, group) if group else user
        cmd.extend(['-u', user_group])

    for g in group_add:
        cmd.extend(['--group-add', g])

    if privileged:
        cmd.append('--privileged')

    for port in publish:
        cmd.extend(['-p', '{0}:{0}'.format(port)])

    if cpuset_cpus:
        cmd.extend(['--cpuset-cpus', cpuset_cpus])

    cmd.extend(run_params)
    cmd.append(image)

    cmd = format_command(cmd, command, docker_host)

    if detach or output:
        return subprocess.check_output(cmd, stdin=stdin, stderr=stderr).decode(
            'utf-8').strip()

    return subprocess.call(cmd, stdin=stdin, stderr=stderr, stdout=stdout)


def exec_(container, command, docker_host=None, user=None, group=None,
          detach=False, interactive=False, tty=False, privileged=False,
          output=False, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    cmd.append('exec')

    if user:
        user_group = '{0}:{1}'.format(user, group) if group else user
        cmd.extend(['-u', user_group])

    if detach:
        cmd.append('-d')

    if detach or sys.__stdin__.isatty():
        if interactive:
            cmd.append('-i')
        if tty:
            cmd.append('-t')

    if privileged:
        cmd.append('--privileged')

    cmd.append(container)

    cmd = format_command(cmd, command, docker_host)

    if detach or output:
        return subprocess.check_output(cmd, stdin=stdin, stderr=stderr).decode(
            'utf-8').strip()

    return subprocess.call(cmd, stdin=stdin, stderr=stderr, stdout=stdout)


def inspect(container, docker_host=None):
    cmd = ['docker']

    cmd.extend(['inspect', container])

    if docker_host:
        cmd = wrap_in_ssh_call(cmd, docker_host)

    out = subprocess.check_output(cmd, universal_newlines=True)
    return json.loads(out)[0]


def logs(container, docker_host=None):
    cmd = ['docker']

    cmd.extend(['logs', container])

    if docker_host:
        cmd = wrap_in_ssh_call(cmd, docker_host)

    return subprocess.check_output(cmd, universal_newlines=True,
                                   stderr=subprocess.STDOUT)


def remove(containers, docker_host=None, force=False,
           link=False, volumes=False):
    cmd = ['docker']

    cmd.append('rm')

    if force:
        cmd.append('-f')

    if link:
        cmd.append('-l')

    if volumes:
        cmd.append('-v')

    if isinstance(containers, str):
        cmd.append(containers)
    else:
        cmd.extend(containers)

    if docker_host:
        cmd = wrap_in_ssh_call(cmd, docker_host)

    subprocess.check_call(cmd)


def cp(container, src_path, dest_path, to_container=False, docker_host=None):
    """Copying file between docker container and host
    :param container: str, docker id or name
    :param src_path: str
    :param dest_path: str
    :param to_container: bool, if True file will be copied from host to
    container, otherwise from docker container to host
    :param docker_host: dict
    """
    cmd = ["docker", "cp"]
    if to_container:
        cmd.extend([src_path, "{0}:{1}".format(container, dest_path)])
    else:
        cmd.extend(["{0}:{1}".format(container, src_path), dest_path])

    if docker_host:
        cmd = wrap_in_ssh_call(cmd, docker_host)

    subprocess.check_call(cmd)


def login(user, password, repository='hub.docker.com'):
    """Logs into docker repository."""

    subprocess.check_call(['docker', 'login', '-u', user, '-p', password,
                           repository])


def build_image(image, build_args):
    """Builds and tags docker image."""

    subprocess.check_call(['docker', 'build', '--no-cache', '--force-rm', '-t',
                           image] + build_args)


def tag_image(image, tag):
    """Tags docker image."""

    subprocess.check_call(['docker', 'tag', image, tag])


def push_image(image):
    """Pushes docker image to the repository."""

    subprocess.check_call(['docker', 'push', image])


def pull_image(image):
    """Pulls docker image from the repository."""

    subprocess.check_call(['docker', 'pull', image])


def remove_image(image):
    """Removes docker image."""

    subprocess.check_call(['docker', 'rmi', '-f', image])


def create_volume(path, name, image, command):
    cmd = ['docker']

    cmd.append('create')
    cmd.append('-v')
    cmd.append(path)

    cmd.append('--name')
    cmd.append(name)

    cmd.append(image)

    cmd.append(command)

    return subprocess.check_output(cmd, universal_newlines=True,
                                   stderr=subprocess.STDOUT)


def ps(all=False, quiet=False):
    """
    List containers
    """
    cmd = ["docker", "ps"]
    if all:
        cmd.append("--all")
    if quiet:
        cmd.append("--quiet")
    return subprocess.check_output(cmd, universal_newlines=True).split()


def list_volumes(quiet=True):
    """
    List volumes
    """
    cmd = ["docker", "volume", "ls"]
    if quiet:
        cmd.append("--quiet")
    return subprocess.check_output(cmd,  universal_newlines=True).split()


def remove_volumes(volumes):
    """
    Remove volumes
    """
    cmd = ["docker", "volume", "rm"]
    if isinstance(volumes, str):
        cmd.append(volumes)
    else:
        cmd.extend(volumes)
    return subprocess.check_call(cmd)


def connect_docker_to_network(network, container):
    """
    Connect docker to the network
    Useful when dockers are in different subnetworks and they need to see each other using IP address
    """

    subprocess.check_call(['docker', 'network', 'connect', network, container])


def format_command(docker_cmd, entry_point, docker_host):
    if isinstance(entry_point, basestring) and docker_host is not None:
        docker_cmd.extend(['sh', '-c', '\"' + entry_point + '\"'])
    elif isinstance(entry_point, basestring):
        docker_cmd.extend(['sh', '-c', entry_point])
    elif isinstance(entry_point, list):
        docker_cmd.extend(entry_point)
    elif entry_point is not None:
        raise ValueError('{0} is not a string nor list'.format(entry_point))

    if docker_host:
        docker_cmd = wrap_in_ssh_call(docker_cmd, docker_host)

    return docker_cmd


def wrap_in_ssh_call(docker_cmd, docker_host):
    username = docker_host['ssh_username']
    hostname = docker_host['ssh_hostname']
    port = docker_host['ssh_port'] if 'ssh_port' in docker_host else 22
    return ['ssh', '-p', port, '{0}@{1}'.format(username, hostname), ' '.join(docker_cmd)]