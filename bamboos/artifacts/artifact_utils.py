"""
This file contains utility functions for scripts responsible for pushing
and pulling build artifacts.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import os
import time

ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'
PARTIAL_EXT = '.partial'
DEFAULT_BRANCH = 'develop'


def lock_file(ssh, file_name):
    """
    Set lock on file_name via ssh. Hangs if file_name is currently locked.
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param file_name: name of file to be locked
    :type file_name: str
    :return None
    """
    _, stdout, _ = ssh.exec_command("lockfile {}.lock".format(file_name),
                                    get_pty=True)
    while not stdout.channel.exit_status_ready():
        # ssh.exec_command is asynchronous therefore we must wait till command
        # exit status is ready
        time.sleep(1)


def unlock_file(ssh, file_name):
    """
    Delete lock on file_name via ssh.
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param file_name: name of file to be unlocked
    :type file_name: str
    :return None
    """
    delete_file(ssh, "{}.lock".format(file_name))


def artifact_path(plan, branch):
    """
    Returns path to artifact for specific plan and branch. Path is relative
    to user's home directory on repository machine.
    :param plan: name of current bamboo plan
    :type plan: str
    :param branch: name of current git branch
    :type branch: str
    """
    return os.path.join(ARTIFACTS_DIR, plan, branch + ARTIFACTS_EXT)


def delete_file(ssh, file_name):
    """
    Delete file named file_name via ssh.
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param file_name: name of file to be unlocked
    :type file_name: str
    :return None
    """

    ssh.exec_command("rm -rf {}".format(file_name))

