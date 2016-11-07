#! /usr/bin/env python
"""
Pushes build artifact to external repo.
Artifact should be file with extension .tar.gz

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import signal
import sys
import argparse
from paramiko import SSHClient, AutoAddPolicy
from scp import SCPClient
from artifact_utils import (lock_file, unlock_file, artifact_path,
                            PARTIAL_EXT, delete_file)


def upload_artifact_safe(ssh, artifact, plan, branch):

    file_name = artifact_path(plan, branch)
    partial_file_name = file_name + PARTIAL_EXT

    def signal_handler(_signum, _frame):
        ssh.connect(args.hostname, port=args.port, username=args.username)
        unlock_file(ssh, partial_file_name)
        delete_file(ssh, partial_file_name)
        unlock_file(ssh, file_name)
        sys.exit(1)
    signal.signal(signal.SIGINT, signal_handler)

    lock_file(ssh, partial_file_name)
    try:
        upload_artifact(ssh, artifact, plan, branch)
        rename_uploaded_file(ssh, file_name)
    except:
        print "Uploading artifact of plan {0}, on branch {1} failed" \
            .format(plan, branch)
        delete_file(ssh, partial_file_name)
    finally:
        unlock_file(ssh, partial_file_name)


def upload_artifact(ssh, artifact, plan, branch):
    """
    Uploads given artifact to repo.
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param artifact: name of artifact to be pushed
    :type artifact: str
    :param plan: name of current bamboo plan
    :type plan: str
    :param branch: name of current git branch
    :type branch: str
    :return None
    """
    with SCPClient(ssh.get_transport()) as scp:
        scp.put(artifact, remote_path=artifact_path(plan, branch) + PARTIAL_EXT)


def rename_uploaded_file(ssh, file_name):
    lock_file(ssh, file_name)
    ssh.exec_command("mv {0}{1} {0}".format(file_name, PARTIAL_EXT))
    unlock_file(ssh, file_name)


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Push build artifacts.')

parser.add_argument(
    '--hostname', '-hn',
    action='store',
    help='Hostname of artifacts repository',
    dest='hostname',
    required=True)

parser.add_argument(
    '--port', '-p',
    action='store',
    type=int,
    help='SSH port to connect to',
    dest='port',
    required=True)

parser.add_argument(
    '--username', '-u',
    action='store',
    help='The username to authenticate as',
    dest='username',
    required=True)

parser.add_argument(
    '--artifact', '-a',
    action='store',
    help='Artifact to be pushed. It should be file with .tar.gz extension',
    dest='artifact',
    required=True)

parser.add_argument(
    '--branch', '-b',
    action='store',
    help='Name of current git branch',
    dest='branch',
    required=True)

parser.add_argument(
    '--plan', '-pl',
    action='store',
    help='Name of current bamboo plan',
    dest='plan',
    required=True)

args = parser.parse_args()

ssh = SSHClient()
ssh.set_missing_host_key_policy(AutoAddPolicy())
ssh.load_system_host_keys()
ssh.connect(args.hostname, port=args.port, username=args.username)

upload_artifact_safe(ssh, args.artifact, args.plan, args.branch)

ssh.close()
