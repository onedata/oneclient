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
        group_add=[], privileged=False, run_params=[], command=None,
        output=False, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

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

    cmd.extend(run_params)
    cmd.append(image)

    if isinstance(command, basestring):
        cmd.extend(['sh', '-c', command])
    elif isinstance(command, list):
        cmd.extend(command)
    elif command is not None:
        raise ValueError('{0} is not a string nor list'.format(command))

    if detach or output:
        return subprocess.check_output(cmd, stdin=stdin, stderr=stderr).decode(
            'utf-8').strip()

    return subprocess.call(cmd, stdin=stdin, stderr=stderr, stdout=stdout)


def exec_(container, command, docker_host=None, user=None, group=None,
          detach=False, interactive=False, tty=False, privileged=False,
          output=False, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

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

    if isinstance(command, basestring):
        cmd.extend(['sh', '-c', command])
    elif isinstance(command, list):
        cmd.extend(command)
    else:
        raise ValueError('{0} is not a string nor list'.format(command))

    if detach or output:
        return subprocess.check_output(cmd, stdin=stdin, stderr=stderr).decode(
            'utf-8').strip()

    return subprocess.call(cmd, stdin=stdin, stderr=stderr, stdout=stdout)


def inspect(container, docker_host=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

    cmd.extend(['inspect', container])
    out = subprocess.check_output(cmd, universal_newlines=True)
    return json.loads(out)[0]


def logs(container, docker_host=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

    cmd.extend(['logs', container])
    return subprocess.check_output(cmd, universal_newlines=True,
                                   stderr=subprocess.STDOUT)


def remove(containers, docker_host=None, force=False,
           link=False, volumes=False):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

    cmd.append('rm')

    if force:
        cmd.append('-f')

    if link:
        cmd.append('-l')

    if volumes:
        cmd.append('-v')

    cmd.extend(containers)
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
