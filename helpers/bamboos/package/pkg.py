#!/usr/bin/env python

# coding=utf-8
"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Pushes .tar.gz package archives in onedata's bamboo artifact format:
i. e.
package/
    fedora-23-x86_64
        SRPMS
            cluster-manager-1.0.0.1.ge1a52f4-1.fc23.src.rpm
        x86_64
            cluster-manager-1.0.0.1.ge1a52f4-1.fc23.x86_64.rpm
    wily
        binary-amd64
            cluster-manager_1.0.0.1.ge1a52f4-1_amd64.deb
        source
            cluster-manager_1.0.0.1.ge1a52f4-1.diff.gz
            cluster-manager_1.0.0.1.ge1a52f4-1.dsc
            cluster-manager_1.0.0.1.ge1a52f4-1_amd64.changes
            cluster-manager_1.0.0.1.ge1a52f4.orig.tar.gz

Available distributions wily, xenial, fedora-21-x86_64, fedora-23-x86_64, centos-7-x86_64, sl6x-x86_64
"""
import argparse
import json
import os
import shutil
import sys
import tempfile
from subprocess import Popen, PIPE, check_call, check_output, CalledProcessError

CONFIG = '''
Host docker_packages_devel
 HostName 172.17.0.2
 User root
 ProxyCommand ssh packages_devel nc %h %p
 StrictHostKeyChecking no
 UserKnownHostsFile=/dev/null

Host docker_packages
 HostName 172.17.0.2
 User root
 ProxyCommand ssh packages nc %h %p
 StrictHostKeyChecking no
 UserKnownHostsFile=/dev/null

Host packages_devel
 HostName 149.156.11.4
 Port 10107
 User ubuntu
 StrictHostKeyChecking no
 UserKnownHostsFile=/dev/null

Host packages
 HostName 149.156.11.4
 Port 10039
 User ubuntu
 StrictHostKeyChecking no
 UserKnownHostsFile=/dev/null
 '''

APACHE_PREFIX = '/var/www/onedata'
YUM_REPO_LOCATION = {
    'fedora-21-x86_64': 'yum/fedora/21',
    'fedora-23-x86_64': 'yum/fedora/23',
    'centos-7-x86_64': 'yum/centos/7x',
    'sl6x-x86_64': 'yum/scientific/6x'
}
DEB_PKG_LOCATION = {
    'trusty': 'apt/ubuntu/trusty/pool/main',
    'wily': 'apt/ubuntu/wily/pool/main',
    'xenial': 'apt/ubuntu/xenial/pool/main'
}
REPO_TYPE = {
    'trusty': 'deb',
    'wily': 'deb',
    'xenial': 'deb',
    'fedora-21-x86_64': 'rpm',
    'fedora-23-x86_64': 'rpm',
    'centos-7-x86_64': 'rpm',
    'sl6x-x86_64': 'rpm'
}

# create the top-level parser
parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Manage package repository.')
subparsers = parser.add_subparsers(
    help='Available actions',
    dest='action'
)

parser.add_argument(
    '--host',
    default=None,
    action='store',
    help='[user@]hostname to connect with package repo. In ssh format.',
    dest='host')

parser.add_argument(
    '-i', '--identity',
    default=None,
    action='store',
    help='Private key.',
    dest='identity')

# create the parser for the "config" command
parser_config = subparsers.add_parser(
    'config',
    help='Print ssh config for onedata package repositories'
)

# create the parser for the "push" command
parser_push = subparsers.add_parser(
    'push',
    help='Deploy .tar.gz package artifact.'
)
parser_push.add_argument(
    'package_artifact',
    help='Package artifact in tar.gz format'
)

# create the parser for the "pull" command
parser_pull = subparsers.add_parser(
    'pull',
    help='Pull packages and create .tar.gz archive.'
)
parser_pull.add_argument(
    'report_artifact',
    help='Report artifact from push command.'
)

args = parser.parse_args()
identity_opt = ['-i', args.identity] if args.identity else []


def cp_or_scp(hostname, identity_opt, source, dest_dir, from_local=True):
    scp_command = ['cp', source, dest_dir]
    if hostname:
        if from_local:
            scp_command = ['scp'] + identity_opt + \
                          [source, hostname + ':' + dest_dir]
        else:
            scp_command = ['scp'] + identity_opt + \
                          [hostname + ':' + source, dest_dir]
    check_call(scp_command, stdout=sys.stdout, stderr=sys.stderr)


def ssh_or_sh(hostname, identity_opt, command, return_output=False):
    ssh_command = ['ssh'] + identity_opt + [hostname] if hostname else []
    if return_output:
        return check_output(ssh_command + command)
    else:
        return check_call(ssh_command + command, stdout=sys.stdout,
                          stderr=sys.stderr)


def untar_remote_or_local(hostname, identity_opt, package_artifact, dest_dir):
    ssh_command = ['ssh'] + identity_opt + [hostname] if hostname else []
    tar_stream = Popen(['cat', package_artifact], stdout=PIPE)
    check_call(ssh_command + ['tar', 'xzf', '-', '-C', dest_dir],
               stdin=tar_stream.stdout)
    tar_stream.wait()


def copy(source, dest_dir, from_local=True):
    cp_or_scp(args.host, identity_opt, source, dest_dir, from_local)


def call(command):
    return ssh_or_sh(args.host, identity_opt, command, True)


def execute(command):
    return ssh_or_sh(args.host, identity_opt, command)


def untar(package_artifact, dest_dir):
    untar_remote_or_local(args.host, identity_opt, package_artifact, dest_dir)


def tar(dir, archive=tempfile.mktemp('.tar.gz')):
    check_call(['tar', 'czf', archive, '-C', dir, '.'])
    return archive


def deb_package_path(distro, type, package):
    name = package.split('_')[0]
    return (os.path.join(DEB_PKG_LOCATION[distro], name[0], name, package),
            os.path.join(distro, type))


def yum_package_path(distro, type, package):
    return (os.path.join(YUM_REPO_LOCATION[distro], type, package),
            os.path.join(distro, type))


def write_report(packages):
    packages = filter(lambda package: not package[0].endswith('.changes'),
                      packages)

    with open('pkg-list.json', 'w') as f:
        json.dump(dict(packages), f, indent=2)


def push(package_artifact):
    tmp_dir = tempfile.mktemp()
    pkg_dir = os.path.join(tmp_dir, 'package')

    try:
        # extract package_artifact
        execute(['rm', '-rf', tmp_dir])
        execute(['mkdir', '-p', tmp_dir])
        untar(package_artifact, tmp_dir)
        packages = []

        # for each distribution inside
        for distro in call(['ls', pkg_dir]).split():
            if REPO_TYPE[distro] == 'deb':
                # push debs
                binary_dir = os.path.join(pkg_dir, distro, 'binary-amd64')
                for package in call(['ls', binary_dir]).split():
                    path = deb_package_path(distro, 'binary-amd64', package)
                    packages.append(path)
                execute(['aptly', 'repo', 'add', '-force-replace', distro,
                         binary_dir])

                # push sources
                source_dir = os.path.join(pkg_dir, distro, 'source')
                for package in call(['ls', source_dir]).split():
                    path = deb_package_path(distro, 'source', package)
                    packages.append(path)
                execute(['aptly', 'repo', 'add', '-force-replace', distro,
                         source_dir])

                # update repo
                execute(['aptly', 'publish', 'update', '-force-overwrite',
                         distro, distro])
            elif REPO_TYPE[distro] == 'rpm':
                # copy packages
                repo_dir = os.path.join(APACHE_PREFIX,
                                        YUM_REPO_LOCATION[distro])
                distro_contents = os.path.join(pkg_dir, distro)

                print("Signing packages ...")
                call(['find', distro_contents, '-name', '*.rpm', '-exec', 'rpmresign',
                      '{}', '\';\''])

                print("Copying packages ...")
                call(['cp', '-a', os.path.join(distro_contents, '.'), repo_dir])

                for type in ['x86_64', 'SRPMS']:
                    dir = os.path.join(distro_contents, type)
                    for package in call(['ls', dir]).split():
                        path = yum_package_path(distro, type, package)
                        packages.append(path)

                # update createrepo
                print("Updating repository ...")
                call(['createrepo', repo_dir])

        write_report(packages)
        return 0

    except CalledProcessError as err:
        return err.returncode
    finally:
        execute(['rm', '-rf', tmp_dir])


def pull(report_artifact):
    tmp_dir = tempfile.mkdtemp()
    pkg_dir = os.path.join(tmp_dir, 'package')

    try:
        with open(report_artifact, 'r') as f:
            report = json.load(f)
            for package_path, distro_dir in report.items():
                source = os.path.join(APACHE_PREFIX, package_path)
                dest = os.path.join(pkg_dir, distro_dir)
                if not os.path.exists(dest):
                    os.makedirs(dest)
                copy(source, dest, False)

        archive = tar(tmp_dir)
        return 0, archive

    except CalledProcessError as err:
        return err.returncode, None
    finally:
        shutil.rmtree(tmp_dir)


if __name__ == '__main__':
    exit_code = 0

    if args.action == 'config':
        print(CONFIG)
    elif args.action == 'push':
        exit_code = push(args.package_artifact)
    elif args.action == 'pull':
        exit_code, archive = pull(args.report_artifact)
        if exit_code == 0:
            print(archive)

    sys.exit(exit_code)
