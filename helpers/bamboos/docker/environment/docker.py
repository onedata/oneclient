# coding=utf-8
"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Functions wrapping capabilities of docker binary.
"""

import json
import os
import sys
import subprocess


# noinspection PyDefaultArgument
def run(image, docker_host=None, detach=False, dns_list=[], envs={},
        hostname=None, interactive=False, link={}, tty=False, rm=False,
        reflect=[], volumes=[], name=None, workdir=None, user=None,
        run_params=[], command=None, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

    cmd.append('run')

    if detach:
        cmd.append('-d')

    for addr in dns_list:
        cmd.extend(['--dns', addr])

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

    for entry in volumes:
        if isinstance(entry, tuple):
            path, bind, readable = entry
            vol = '{0}:{1}:{2}'.format(os.path.abspath(path), bind, readable)
            cmd.extend(['-v', vol])
        else:
            cmd.extend(['-v', entry])

    if workdir:
        cmd.extend(['-w', os.path.abspath(workdir)])

    if user:
        cmd.extend(['-u', user])

    cmd.extend(run_params)
    cmd.append(image)

    if isinstance(command, str):
        cmd.extend(['sh', '-c', command])
    elif isinstance(command, list):
        cmd.extend(command)

    if detach:
        return subprocess.check_output(cmd, stdin=stdin, stderr=stderr).decode(
            'utf-8').strip()

    return subprocess.call(cmd, stdin=stdin, stderr=stderr, stdout=stdout)


def exec_(container, command, docker_host=None, detach=False, interactive=False,
          tty=False, output=False, stdin=None, stdout=None, stderr=None):
    cmd = ['docker']

    if docker_host:
        cmd.extend(['-H', docker_host])

    cmd.append('exec')

    if detach:
        cmd.append('-d')

    if detach or sys.__stdin__.isatty():
        if interactive:
            cmd.append('-i')
        if tty:
            cmd.append('-t')

    cmd.append(container)

    if isinstance(command, str):
        cmd.extend(['sh', '-c', command])
    elif isinstance(command, list):
        cmd.extend(command)

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
