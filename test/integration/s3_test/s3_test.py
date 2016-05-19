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
from boto.s3.connection import S3Connection, OrdinaryCallingFormat
import s3


@pytest.fixture(scope='module')
def s3_client(request):
    class S3Client(object):
        def __init__(self, scheme, host_name, access_key, secret_key, bucket):
            [ip, port] = host_name.split(':')
            self.scheme = scheme
            self.host_name = host_name
            self.access_key = access_key
            self.secret_key = secret_key
            self.bucket = bucket
            self.conn = S3Connection(self.access_key, self.secret_key,
                                     host=ip, port=int(port), is_secure=False,
                                     calling_format=OrdinaryCallingFormat())

        def list_bucket(self, file_id):
            bucket = self.conn.get_bucket(self.bucket, validate=False)
            return list(bucket.list(prefix=file_id + '/', delimiter='/'))

    bucket = 'data'
    result = s3_server.up('onedata/s3proxy', [bucket], 'storage', '1')
    [container] = result['docker_ids']

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    return S3Client('http', result['host_name'], result['access_key'], result[
        'secret_key'], bucket)


@pytest.fixture
def helper(s3_client):
    return s3.S3Proxy(s3_client.scheme, s3_client.host_name, s3_client.bucket,
                      s3_client.access_key, s3_client.secret_key)


def test_write_should_write_empty_data(helper):
    file_id = random_str()
    data = ''
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)


def test_write_should_write_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)


def test_write_should_append_data(helper, s3_client):
    file_id = random_str()
    block_num = 10
    block_size = 5
    data = ''

    for i in range(block_num):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data += block

    assert helper.read(file_id, 0, block_num * block_size) == data
    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_prepend_data(helper, s3_client):
    file_id = random_str()
    block_num = 10
    block_size = 5
    data = ''

    for i in range(block_num - 1, -1, -1):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data = block + data

    assert helper.read(file_id, 0, block_num * block_size) == data
    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_merge_data(helper, s3_client):
    file_id = random_str()
    block_num = 10
    block_size = 5
    data = [None] * block_num

    for i in range(0, block_num, 2):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data[i] = block

    for i in range(1, block_num, 2):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data[i] = block

    assert helper.read(file_id, 0, block_num * block_size) == ''.join(data)
    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_overwrite_data_left(helper, s3_client):
    file_id = random_str()
    size = 10

    for block_size in range(0, size):
        data = random_str(block_size)
        assert helper.write(file_id, data, 0) == len(data)
        assert helper.read(file_id, 0, len(data)) == data

    for block_size in range(size, -1, -1):
        data = random_str(block_size)
        assert helper.write(file_id, data, 0) == len(data)
        assert helper.read(file_id, 0, len(data)) == data

    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_overwrite_data_right(helper, s3_client):
    file_id = random_str()
    size = 10

    for block_size in range(size, -1, -1):
        data = random_str(block_size)
        assert helper.write(file_id, data, size - block_size) == len(data)
        assert helper.read(file_id, size - block_size, len(data)) == data

    for block_size in range(size, -1, -1):
        data = random_str(size - block_size)
        assert helper.write(file_id, data, size) == len(data)
        assert helper.read(file_id, size, len(data)) == data

    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_overwrite_data_middle(helper, s3_client):
    file_id = random_str()
    size = 10

    for block_size in range(size / 2):
        data = random_str(2 * block_size)
        assert helper.write(file_id, data, size - block_size) == len(data)
        assert helper.read(file_id, size - block_size, len(data)) == data

    for block_size in range(size / 2, -1, -1):
        data = random_str(2 * block_size)
        assert helper.write(file_id, data, size / 2 - block_size) == len(data)
        assert helper.read(file_id, size / 2 - block_size, len(data)) == data

    assert len(s3_client.list_bucket(file_id)) == 1


def test_write_should_write_big_block(helper, s3_client):
    file_id = random_str()
    mb = 100
    seed = random_str(1024)
    data = seed * 1024 * mb

    assert helper.write(file_id, data, 0) == len(data)
    assert helper.read(file_id, 0, len(data)) == data
    assert len(s3_client.list_bucket(file_id)) == 20


def test_write_should_overwrite_big_block_part(helper, s3_client):
    file_id = random_str()
    mb = 10
    updates_num = 100
    block_size = 4096
    seed = random_str(1024)
    data = seed * 1024 * mb

    assert helper.write(file_id, data, 0) == len(data)
    for _ in range(updates_num):
        offset = random_int(lower_bound=0, upper_bound=len(data))
        block = random_str(block_size)
        data = data[:offset] + block + data[offset + len(block):]
        helper.write(file_id, block, offset) == len(block)
        assert helper.read(file_id, 0, len(data)) == data
        assert len(s3_client.list_bucket(file_id)) == 2


def test_read_should_read_empty_data(helper):
    file_id = random_str()
    offset = random_int()
    size = random_int()

    assert helper.read(file_id, offset, size) == ''


def test_read_should_read_data(helper):
    file_id = random_str()
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, offset, len(data)) == data


def test_read_should_read_data_with_holes(helper):
    file_id = random_str()
    block_num = 10
    block_size = 5
    data = ['\0' * block_size] * block_num

    for i in range(1, block_num, 2):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data[i] = block

    assert helper.read(file_id, 0, block_num * block_size) == ''.join(data)


# def test_read_should_not_read_after_end_of_file(helper):
#     file_id = random_str()
#     data = random_str()
#     offset = random_int()
#
#     assert helper.write(file_id, data, offset) == len(data)
#     assert helper.read(file_id, offset + len(data), random_int()) == ''


def test_read_should_read_empty_segment(helper, s3_client):
    file_id = random_str()
    data = random_str()
    offset = random_int()
    seg_size = random_int()
    seg_offset = offset + len(data) + random_int()

    assert len(s3_client.list_bucket(file_id)) == 0
    assert helper.write(file_id, data, offset) == len(data)
    assert helper.write(file_id, data, seg_offset + seg_size) == len(data)
    assert helper.read(file_id, seg_offset, seg_size) == '\0' * seg_size


# def test_unlink_should_delete_empty_data(helper):
#     file_id = random_str()
#
#     helper.unlink(file_id)
#
#
# def test_unlink_should_delete_data(helper):
#     file_id = random_str()
#     data = random_str()
#     offset = random_int()
#
#     assert helper.write(file_id, data, offset) == len(data)
#     assert helper.read(file_id, offset, len(data)) == data
#     helper.unlink(file_id)
#     assert helper.read(file_id, offset, len(data)) == ''
#
#
# def test_truncate_should_create_empty_file(helper):
#     file_id = random_str()
#
#     for size in range(random_int(), -1, -1):
#         helper.truncate(file_id, size)
#         assert helper.read(file_id, 0, size + 1) == '\0' * size
#
#
# def test_truncate_should_decrease_file_size(helper, s3_client):
#     file_id = random_str()
#     data = random_str()
#
#     assert helper.write(file_id, data, 0) == len(data)
#     for size in range(len(data) - 1, -1, -1):
#         helper.truncate(file_id, size)
#         assert helper.read(file_id, 0, size + 1) == data[:size]
#         assert len(s3_client.list_bucket(file_id)) == int(size > 0)
#
#
# def test_truncate_should_increase_file_size(helper, s3_client):
#     file_id = random_str()
#     data = random_str()
#     file_size = len(data) + random_int()
#
#     assert helper.write(file_id, data, 0) == len(data)
#     data += '\0' * (file_size - len(data))
#
#     for size in range(len(data), file_size):
#         helper.truncate(file_id, size)
#         assert helper.read(file_id, 0, size + 1) == data[:size]
#         assert len(s3_client.list_bucket(file_id)) == 1
