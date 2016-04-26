"""This module tests S3 helper."""

__author__ = "Krzysztof Trzepla"
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
from environment import s3 as s3_server
import s3

ACCESS_KEY = "AKIAJQNCYDS4EAAGR73A"
SECRET_KEY = "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"


@pytest.fixture(scope='module')
def s3_client(request):
    class S3Client(object):
        def __init__(self, host_name, access_key, secret_key, bucket):
            self.host_name = host_name
            self.access_key = access_key
            self.secret_key = secret_key
            self.bucket = bucket

    bucket = 'data'
    result = s3_server.up('lphoward/fake-s3', [bucket], 'storage', '1')
    [container] = result['docker_ids']

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    return S3Client(result['host_name'], result['access_key'], result[
        'secret_key'], bucket)


@pytest.fixture
def helper(s3_client):
    return s3.S3Proxy(s3_client.host_name, s3_client.bucket,
                      s3_client.access_key, s3_client.secret_key)


def test_write_should_write_data(helper):
    file_id = random_str()
    data = random_str()
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)


def test_write_should_modify_data(helper):
    file_id = random_str()
    data = random_str(20)
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)
    offset = random_int(lower_bound=5, upper_bound=15)
    update = random_str(5)
    data = data[:offset] + update + data[offset + 5:]
    assert helper.write(file_id, update, offset) == len(update)
    assert helper.read(file_id, 0, len(data)) == data


def test_write_should_append_data(helper):
    file_id = random_str()
    data = random_str(20)
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)
    offset = random_int(lower_bound=5, upper_bound=15)
    update = random_str(20)
    data = data[:offset] + update
    assert helper.write(file_id, update, offset) == len(update)
    assert helper.read(file_id, 0, len(data)) == data


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


def test_unlink_should_delete_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    helper.unlink(file_id)


def test_truncate_should_truncate_data(helper):
    file_id = random_str()
    data = random_str()
    size = random_int(upper_bound=len(data))

    assert helper.write(file_id, data, 0) == len(data)
    helper.truncate(file_id, size)
    assert helper.read(file_id, 0, size) == data[0:size]
