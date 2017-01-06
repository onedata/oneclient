"""This module tests swift helper."""

__author__ = "Michal Wrona"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
from environment import common, docker, swift
from key_value_test_base import *
from swift_helper import SwiftHelperProxy


@pytest.fixture(scope='module')
def server(request):
    class Server(object):
        def __init__(self, auth_url, container_name, tenant_name, username,
                     password, container, ip):
            self.auth_url = auth_url
            self.container_name = container_name
            self.tenant_name = tenant_name
            self.username = username
            self.password = password
            self.container = container
            self.ip = ip

        def list(self, file_id):
            cmd = 'list {0} -p {1}'.format(self.container_name, file_id)
            cmd = swift.SWIFT_COMMAND.format(self.ip, cmd).split()
            res = docker.exec_(self.container, cmd, output=True, stdout=sys.stderr)
            return res.split()

    container_name = 'onedata'
    result = swift.up('onedata/dockswift', [container_name], 'storage',
                      common.generate_uid())
    [container] = result['docker_ids']
    auth_url = 'http://{0}:{1}/v2.0/tokens'.format(result['host_name'],
                                                   result['keystone_port'])

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    print("AuthUrl: {0}".format(auth_url))

    return Server(auth_url, container_name, result['tenant_name'],
                  result['user_name'], result['password'], container,
                  result['host_name'])


@pytest.fixture
def helper(request, server):
    return SwiftHelperProxy(server.auth_url, server.container_name,
                            server.tenant_name, server.username,
                            server.password, THREAD_NUMBER, BLOCK_SIZE)
