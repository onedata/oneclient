"""This module contains test for KeyValue helpers."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

from test_common import *
import pytest

@pytest.fixture
def file_id():
    return random_str(32)

def test_write_should_write_empty_data(helper, file_id):
    data = ''
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)


def test_write_should_write_data(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)


def test_write_should_append_data(helper, file_id):
    block_num = 10
    block_size = 5
    data = ''

    for i in range(block_num):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data += block

    assert helper.read(file_id, 0, block_num * block_size) == data


def test_write_should_prepend_data(helper, file_id):
    block_num = 10
    block_size = 5
    data = ''

    for i in range(block_num - 1, -1, -1):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data = block + data

    assert helper.read(file_id, 0, block_num * block_size) == data


def test_write_should_merge_data(helper, file_id):
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


def test_write_should_overwrite_data_left(helper, file_id):
    size = 10

    for block_size in range(0, size):
        data = random_str(block_size)
        assert helper.write(file_id, data, 0) == len(data)
        assert helper.read(file_id, 0, len(data)) == data

    for block_size in range(size, -1, -1):
        data = random_str(block_size)
        assert helper.write(file_id, data, 0) == len(data)
        assert helper.read(file_id, 0, len(data)) == data


def test_write_should_overwrite_data_right(helper, file_id):
    size = 10

    for block_size in range(size, -1, -1):
        data = random_str(block_size)
        assert helper.write(file_id, data, size - block_size) == len(data)
        assert helper.read(file_id, size - block_size, len(data)) == data

    for block_size in range(size, -1, -1):
        data = random_str(size - block_size)
        assert helper.write(file_id, data, size) == len(data)
        assert helper.read(file_id, size, len(data)) == data


def test_write_should_overwrite_data_middle(helper, file_id):
    size = 10

    for block_size in range(size / 2):
        data = random_str(2 * block_size)
        assert helper.write(file_id, data, size - block_size) == len(data)
        assert helper.read(file_id, size - block_size, len(data)) == data

    for block_size in range(size / 2, -1, -1):
        data = random_str(2 * block_size)
        assert helper.write(file_id, data, size / 2 - block_size) == len(data)
        assert helper.read(file_id, size / 2 - block_size, len(data)) == data


def test_read_shoud_not_read_data(helper, file_id):
    data = random_str()

    helper.write(file_id, data, 0) == len(data)
    for offset in range(len(data)):
        assert helper.read(file_id, offset, 0) == ''


def test_read_should_read_data(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, offset, len(data)) == data


def test_read_should_read_all_possible_ranges(helper, file_id):
    data = random_str(10)
    offset = 0

    assert helper.write(file_id, data, offset) == len(data)
    for offset in range(len(data)):
        for size in range(len(data) - offset):
            assert helper.read(file_id, offset, size) == \
                   data[offset:offset + size]


def test_read_should_pad_prefix_with_zeros(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, 0, len(data) + offset) == '\0' * offset + data


def test_read_should_read_data_with_holes(helper, file_id):
    block_num = 10
    block_size = 5
    data = ['\0' * block_size] * block_num

    for i in range(1, block_num, 2):
        block = random_str(block_size)
        offset = i * block_size
        assert helper.write(file_id, block, offset) == len(block)
        data[i] = block

    assert helper.read(file_id, 0, block_num * block_size) == ''.join(data)


def test_read_should_not_read_after_end_of_file(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, offset + len(data), random_int()) == ''


def test_read_should_read_empty_segment(helper, file_id):
    data = random_str()
    offset = random_int()
    seg_size = random_int()
    seg_offset = offset + len(data) + random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.write(file_id, data, seg_offset + seg_size) == len(data)
    assert helper.read(file_id, seg_offset, seg_size) == '\0' * seg_size


def test_unlink_should_delete_empty_data(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    helper.unlink(file_id)


def test_truncate_should_decrease_file_size(helper, file_id):
    data = random_str()

    assert helper.write(file_id, data, 0) == len(data)
    for size in range(len(data) - 1, -1, -1):
        helper.truncate(file_id, size)
        assert helper.read(file_id, 0, size + 1) == data[:size]


def test_truncate_should_increase_file_size(helper, file_id):
    data = random_str()
    file_size = len(data) + random_int()

    assert helper.write(file_id, data, 0) == len(data)
    data += '\0' * (file_size - len(data))

    for size in range(len(data), file_size):
        helper.truncate(file_id, size)
        assert helper.read(file_id, 0, size + 1) == data[:size]
