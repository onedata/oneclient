"""This module contains test for KeyValue helpers."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

from test_common import *
from common_test_base import *
import pytest

THREAD_NUMBER = 8
BLOCK_SIZE = 100

def test_write_should_write_multiple_blocks(helper, file_id, server):
    block_num = 20
    seed = random_str(BLOCK_SIZE)
    data = seed * block_num

    assert helper.write(file_id, data, 0) == len(data)
    assert helper.read(file_id, 0, len(data)) == data
    assert len(server.list(file_id)) == block_num


def test_unlink_should_delete_data(helper, file_id, server):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert len(server.list(file_id)) > 0
    helper.unlink(file_id)
    assert len(server.list(file_id)) == 0


def test_truncate_should_create_empty_file(helper, file_id):
    for size in range(random_int(), -1, -1):
        helper.truncate(file_id, size)
        assert helper.read(file_id, 0, size + 1) == '\0' * size


def test_truncate_should_create_empty_multi_block_file(helper, file_id, server):
    blocks_num = 10
    size = blocks_num * BLOCK_SIZE

    helper.truncate(file_id, size)
    assert helper.read(file_id, 0, size + 1) == '\0' * size
    assert len(server.list(file_id)) == 1


def test_truncate_should_pad_block(helper, file_id, server):
    data = random_str()

    assert helper.write(file_id, data, BLOCK_SIZE) == len(data)
    assert len(server.list(file_id)) == 1
    helper.truncate(file_id, BLOCK_SIZE)
    assert helper.read(file_id, 0, BLOCK_SIZE + 1) == '\0' * BLOCK_SIZE
    assert helper.write(file_id, data, BLOCK_SIZE) == len(data)


def test_truncate_should_delete_all_blocks(helper, file_id, server):
    blocks_num = 10
    data = random_str(blocks_num * BLOCK_SIZE)

    assert helper.write(file_id, data, 0) == len(data)
    assert len(server.list(file_id)) == blocks_num
    helper.truncate(file_id, 0)
    assert helper.read(file_id, 0, len(data)) == ''
    assert len(server.list(file_id)) == 0


def test_write_should_overwrite_multiple_blocks_part(helper, file_id):
    block_num = 10
    updates_num = 100
    seed = random_str(BLOCK_SIZE)
    data = seed * block_num

    assert helper.write(file_id, data, 0) == len(data)
    for _ in range(updates_num):
        offset = random_int(lower_bound=0, upper_bound=len(data))
        block = random_str(BLOCK_SIZE)
        data = data[:offset] + block + data[offset + len(block):]
        helper.write(file_id, block, offset) == len(block)
        assert helper.read(file_id, 0, len(data)) == data


def test_read_should_read_multi_block_data_with_holes(helper, file_id):
    data = random_str(10)
    empty_block = '\0' * BLOCK_SIZE
    block_num = 10

    assert helper.write(file_id, data, 0) == len(data)
    assert helper.write(file_id, data, block_num * BLOCK_SIZE) == len(data)

    data = data + empty_block[len(data):] + (block_num - 1) * empty_block + data
    assert helper.read(file_id, 0, len(data)) == data


def test_read_should_read_empty_data(helper, file_id):
    offset = random_int()
    size = random_int()

    assert helper.read(file_id, offset, size) == ''
