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
from environment import docker
from environment import swift as swift_server
from key_value_test_base import *
import swift


@pytest.fixture(scope='module')
def client(request):
    class SwiftClient(object):
        def __init__(self, auth_url, container_name, tenant_name, user_name,
                     password, container, ip):
            self.auth_url = auth_url
            self.container_name = container_name
            self.tenant_name = tenant_name
            self.user_name = user_name
            self.password = password
            self.container = container
            self.ip = ip

        def list(self, file_id):
            res = docker.exec_(self.container,
                               swift_server.SWIFT_COMMAND.format(
                                   self.ip, 'list {0} -p {1}'.format(
                                       self.container_name, file_id)).split(),
                               output=True, stdout=sys.stderr)
            return res.split()

    container_name = 'onedata'
    result = swift_server.up('onedata/dockswift', [container_name],
                             'storage', '1')
    [container] = result['docker_ids']

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    return SwiftClient('http://{0}:{1}/v2.0/tokens'.format(result['host_name'],
                                                           result[
                                                               'keystone_port']),
                       container_name, result['tenant_name'],
                       result['user_name'], result['password'],
                       container, result['host_name'])


@pytest.fixture
def helper(request, client):
    return swift.SwiftProxy(client.auth_url, client.container_name,
                            client.tenant_name, client.user_name,
                            client.password, BLOCK_SIZE)

