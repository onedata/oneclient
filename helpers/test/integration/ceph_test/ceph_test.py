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
from environment import common, docker
import ceph


@pytest.fixture
def helper(ceph_client):
    return ceph.CephProxy(ceph_client.mon_host, ceph_client.username,
                          ceph_client.key, ceph_client.pool_name)


def test_write_should_write_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)


def test_read_should_pass_errors(helper):
    file_id = random_str()
    offset = random_int()
    size = random_int()

    with pytest.raises(RuntimeError) as excinfo:
        helper.read(file_id, offset, size)
    assert 'No such file or directory' in str(excinfo.value)


def test_read_should_read_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, offset, len(data)) == data


def test_unlink_should_pass_errors(helper):
    file_id = random_str()

    with pytest.raises(RuntimeError) as excinfo:
        helper.unlink(file_id)
    assert 'No such file or directory' in str(excinfo.value)


def test_unlink_should_unlink_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    helper.unlink(file_id)


def test_truncate_should_truncate_data(helper):
    file_id = random_str()
    data = random_str()
    size = random_int(len(data))

    assert helper.write(file_id, data, 0) == len(data)
    helper.truncate(file_id, size)
    assert helper.read(file_id, 0, size) == data[0:size]
