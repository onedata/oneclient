"""This module tests S3 helper."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

from boto.s3.connection import S3Connection
import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
import s3

ACCESS_KEY = "AKIAJQNCYDS4EAAGR73A"
SECRET_KEY = "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"


@pytest.fixture(scope='module')
def bucket(request):
    class Bucket(object):
        def __init__(self, name):
            self.name = name

    connection = S3Connection(ACCESS_KEY, SECRET_KEY)
    name = random_str(size=20, characters=string.ascii_lowercase)
    connection.create_bucket(name)

    def fin():
        keys = connection.get_bucket(name)
        for key in keys.list():
            key.delete()
        connection.delete_bucket(name)

    request.addfinalizer(fin)

    return Bucket(name)


@pytest.fixture
def helper(bucket):
    return s3.S3Proxy("s3.amazonaws.com", bucket.name,
                      ACCESS_KEY, SECRET_KEY)


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
