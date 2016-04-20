"""This module tests Ceph helper."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
# noinspection PyUnresolvedReferences
from environment import docker
from environment import ceph as ceph_server
import ceph


@pytest.fixture(scope='module')
def ceph_client(request):
    class CephClient(object):
        def __init__(self, mon_host, username, key, pool_name):
            self.mon_host = mon_host
            self.username = username
            self.key = key
            self.pool_name = pool_name

    pool_name = 'data'
    result = ceph_server.up(image='onedata/ceph', pools=[(pool_name, '8')])

    [container] = result['docker_ids']
    username = result['username']
    key = str(result['key'])
    mon_host = docker.inspect(container)['NetworkSettings']['IPAddress']. \
        encode('ascii')

    def fin():
        docker.exec_(container, ['btrfs', 'subvolume', 'delete',
                                 '/var/lib/ceph/osd/ceph-0/current'])
        docker.exec_(container, ['btrfs', 'subvolume', 'delete',
                                 '/var/lib/ceph/osd/ceph-0/snap_1'])
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    return CephClient(mon_host, username, key, pool_name)


@pytest.fixture
def helper(ceph_client):
    return ceph.CephProxy(ceph_client.mon_host, ceph_client.username,
                          ceph_client.key, ceph_client.pool_name)


def test_write_should_write_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()
    parameters = random_params()

    assert helper.write(file_id, data, offset, parameters) == len(data)


def test_read_should_pass_errors(helper):
    file_id = random_str()
    offset = random_int()
    size = random_int()
    parameters = random_params()

    with pytest.raises(RuntimeError) as excinfo:
        helper.read(file_id, offset, size, parameters)
    assert 'No such file or directory' in str(excinfo.value)


def test_read_should_read_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()
    parameters = random_params()

    assert helper.write(file_id, data, offset, parameters) == len(data)
    assert helper.read(file_id, offset, len(data), parameters) == data


def test_unlink_should_pass_errors(helper):
    file_id = random_str()

    with pytest.raises(RuntimeError) as excinfo:
        helper.unlink(file_id)
    assert 'No such file or directory' in str(excinfo.value)


def test_unlink_should_delete_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()
    parameters = random_params()

    assert helper.write(file_id, data, offset, parameters) == len(data)
    helper.unlink(file_id)


def test_truncate_should_truncate_data(helper):
    file_id = random_str()
    data = random_str()
    size = random_int(upper_bound=len(data))
    parameters = random_params()

    assert helper.write(file_id, data, 0, parameters) == len(data)
    helper.truncate(file_id, size)
    assert helper.read(file_id, 0, size, parameters) == data[0:size]
