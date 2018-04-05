"""This module tests read cache prefetch overhead."""

__author__ = "Bartek Kryza"
__copyright__ = """(C) 2018 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import subprocess
import datetime
from os.path import expanduser
from random import sample
from time import sleep

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
# noinspection PyUnresolvedReferences
from environment import common, docker, nfs
from readcache_prefetch import BufferedNullDeviceHelperProxy
from posix_test_types import *

def measureElapsed(start):
    diff = datetime.datetime.now() - start
    return (diff.days * 86400000) + (diff.seconds * 1000)\
            + (diff.microseconds / 1000)

@pytest.fixture
def file_id():
    return random_str(32)

@pytest.fixture(scope='module')
def server(request):
    class Server(object):
        def __init__(self, latencyMin, latencyMax, timeoutProbability, filter):
            self.latencyMin = latencyMin
            self.latencyMax = latencyMax
            self.timeoutProbability = timeoutProbability
            self.filter = filter

    def fin():
         pass

    request.addfinalizer(fin)

    return Server(0, 0, 0.0, "*")

@pytest.fixture(scope='module')
def slowServer(request):
    class Server(object):
        def __init__(self, latencyMin, latencyMax, timeoutProbability, filter):
            self.latencyMin = latencyMin
            self.latencyMax = latencyMax
            self.timeoutProbability = timeoutProbability
            self.filter = filter

    def fin():
         pass

    request.addfinalizer(fin)

    return Server(25, 75, 0.0, "read,write")

@pytest.fixture
def helper(server):
    """
    Create a helper to ideal server
    """
    return BufferedNullDeviceHelperProxy(
        server.latencyMin,
        server.latencyMax,
        server.timeoutProbability,
        server.filter)

@pytest.fixture
def slowStorageHelper(slowServer):
    """
    Create a helper to slow (non-zero latency) server
    """
    return BufferedNullDeviceHelperProxy(
        slowServer.latencyMin,
        slowServer.latencyMax,
        slowServer.timeoutProbability,
        slowServer.filter)


@pytest.mark.readwrite_operations_tests
def test_single_read_after_period_should_invalidate_cache(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = len(helper.read(handle, 0, len(data)))

    assert read == len(data)
    assert helper.readBytes(handle) == read

    sleep(1)

    # Rereading the same file from the cache after 1s should
    # not increase the helper read count
    read = len(helper.read(handle, 0, len(data)))

    assert read == len(data)
    assert helper.readBytes(handle) == read

    sleep(4)

    # Rereading the same file from the cache after 4s should
    # force the helper to read the data again
    read = len(helper.read(handle, 0, len(data)))

    assert read == len(data)
    assert helper.readBytes(handle) == 2*read


@pytest.mark.readwrite_operations_tests
def test_single_read_from_ideal_storage_should_not_generate_overhead(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = len(helper.read(handle, 0, len(data)))

    assert read == len(data)
    assert helper.readBytes(handle) == read


@pytest.mark.readwrite_operations_tests
def test_linear_read_from_ideal_storage_should_prefetch_data(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    for i in range(0, 3):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 3*1024*1024
    assert helper.readBytes(handle) >= 6*1024*1024


@pytest.mark.readwrite_operations_tests
def test_fast_linear_read_from_ideal_storage_should_not_generate_too_big_overhead(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    for i in range(0, 150):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.01)

    assert read == len(data)
    assert helper.readBytes(handle) <= read*1.2


@pytest.mark.readwrite_operations_tests
def test_linear_read_from_ideal_storage_should_not_generate_too_big_overhead(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    for i in range(0, 150):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == len(data)
    assert helper.readBytes(handle) >= read
    assert helper.readBytes(handle) < read*1.2


@pytest.mark.readwrite_operations_tests
def test_random_read_from_ideal_storage_should_not_generate_too_big_overhead(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    blocks = range(0, 150)
    random_blocks = random.sample(blocks, len(blocks))
    for i in random_blocks:
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == len(data)
    assert helper.readBytes(handle) >= read
    assert helper.readBytes(handle) < read*1.2


@pytest.mark.readwrite_operations_tests
def test_mixed_read_from_ideal_storage_should_slow_down_prefetching(helper, file_id):
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    for i in range(0, 5):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    helperReadBytes = helper.readBytes(handle)
    assert helperReadBytes >= 2*5*1024*1024

    read = 0
    blocks = range(100, 150)
    random_blocks = random.sample(blocks, 5)
    for i in random_blocks:
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    helperReadBytes2 = helper.readBytes(handle)
    assert helperReadBytes2 - helperReadBytes < read*1.2

    read = 0
    for i in range(50, 55):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    assert helper.readBytes(handle)-helperReadBytes2 >= 2*5*1024*1024


@pytest.mark.readwrite_operations_tests
def test_linear_read_from_slow_storage_should_prefetch_data(slowStorageHelper, file_id):
    data = 'x' * 1024*1024*150

    handle = slowStorageHelper.open(file_id, 0655)
    written = slowStorageHelper.write(handle, data, 0)

    assert written == len(data)
    assert written == slowStorageHelper.writtenBytes(handle)

    read = 0
    for i in range(0, 3):
        read += len(slowStorageHelper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 3*1024*1024
    assert slowStorageHelper.readBytes(handle) >= 6*1024*1024


@pytest.mark.readwrite_operations_tests
def test_linear_read_from_slow_storage_should_not_generate_too_big_overhead(slowStorageHelper, file_id):
    data = 'x' * 1024*1024*150

    handle = slowStorageHelper.open(file_id, 0655)
    written = slowStorageHelper.write(handle, data, 0)

    assert written == len(data)
    assert written == slowStorageHelper.writtenBytes(handle)

    read = 0
    for i in range(0, 150):
        read += len(slowStorageHelper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == len(data)
    assert slowStorageHelper.readBytes(handle) >= read
    assert slowStorageHelper.readBytes(handle) < read*1.2


def test_random_read_from_slow_storage_should_not_generate_too_big_overhead(slowStorageHelper, file_id):
    data = 'x' * 1024*1024*150

    handle = slowStorageHelper.open(file_id, 0655)
    written = slowStorageHelper.write(handle, data, 0)

    assert written == len(data)
    assert written == slowStorageHelper.writtenBytes(handle)

    read = 0
    blocks = range(0, 150)
    random_blocks = random.sample(blocks, len(blocks))
    for i in random_blocks:
        read += len(slowStorageHelper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == len(data)
    assert slowStorageHelper.readBytes(handle) >= read
    assert slowStorageHelper.readBytes(handle) < read*1.2


@pytest.mark.readwrite_operations_tests
def test_mixed_read_from_slow_storage_should_slow_down_prefetching(slowStorageHelper, file_id):
    helper = slowStorageHelper
    data = 'x' * 1024*1024*150

    handle = helper.open(file_id, 0655)
    written = helper.write(handle, data, 0)

    assert written == len(data)
    assert written == helper.writtenBytes(handle)

    read = 0
    for i in range(0, 5):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    helperReadBytes = helper.readBytes(handle)
    assert helperReadBytes >= 2*5*1024*1024

    read = 0
    blocks = range(100, 150)
    random_blocks = random.sample(blocks, 5)
    for i in random_blocks:
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    helperReadBytes2 = helper.readBytes(handle)
    assert helperReadBytes2 - helperReadBytes < read*1.2

    read = 0
    for i in range(50, 55):
        read += len(helper.read(handle, i*(1024*1024), 1024*1024))
        sleep(0.1)

    assert read == 5*1024*1024
    assert helper.readBytes(handle)-helperReadBytes2 >= 2*5*1024*1024


