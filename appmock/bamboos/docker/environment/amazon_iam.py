# coding=utf-8
"""Author: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a Amazon IAM mock.
"""
import os
import subprocess

from . import docker, common

IAM_MOCK_FILE_NAME = 'amazon_iam_mock.py'


def _node_up(image, uid):
    hostname = common.format_hostname('amazon-iam', uid)
    file_location = os.path.dirname(os.path.realpath(__file__))
    container = docker.run(
        image=image, privileged=True,
        volumes=[(os.path.join(file_location, 'resources', IAM_MOCK_FILE_NAME),
                  '/root/{0}'.format(IAM_MOCK_FILE_NAME), 'ro')],
        tty=True, interactive=True,
        detach=True, command='/root/amazon_iam_mock.py > /tmp/run.log 2>&1',
        name=hostname, hostname=hostname)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {
        'docker_ids': [container],
        'host_name': '{0}:5000'.format(ip)
    }


def up(image, uid):
    return _node_up(image, uid)
