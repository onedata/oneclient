from __future__ import print_function

import random

__author__ = "Bartek Kryza"
__copyright__ = """(C) 2019 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
from threading import Thread
from multiprocessing import Pool
import time
import tempfile
import pytest
from stat import *

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import reply, send, random_str
# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import helperscache
# noinspection PyUnresolvedReferences
from proto import messages_pb2, fuse_messages_pb2, event_messages_pb2, \
    common_messages_pb2, stream_messages_pb2


@pytest.yield_fixture
def endpoint(appmock_client):
    app = appmock_client.tcp_endpoint(443)
    yield app
    appmock_client.reset_tcp_history()


@pytest.yield_fixture
def hc(endpoint):
    cache = helperscache.HelpersCacheProxy(
            endpoint.ip, endpoint.port,
            '--no-buffer --provider-timeout=2 mountpoint')
    yield cache
    cache.stop()


@pytest.yield_fixture
def hc_directio(endpoint):
    cache = helperscache.HelpersCacheProxy(
            endpoint.ip, endpoint.port,
            '--no-buffer --provider-timeout=2 --force-direct-io mountpoint')
    yield cache
    cache.stop()


@pytest.yield_fixture
def hc_proxyio(endpoint):
    cache = helperscache.HelpersCacheProxy(
            endpoint.ip, endpoint.port,
            '--no-buffer --provider-timeout=2 --force-proxy-io mountpoint')
    yield cache
    cache.stop()


@pytest.fixture
def storage_id():
    return random_str()


@pytest.fixture
def space_id():
    return random_str()


@pytest.fixture
def file_uuid():
    return random_str()


@pytest.fixture
def file_content():
    return random_str()


def prepare_posix_helper_params(storage_id, mount_point):
    repl = fuse_messages_pb2.HelperParams()
    repl.helper_name = 'posix'
    p = repl.helper_args.add()
    p.key = 'type'
    p.value = 'posix'
    p = repl.helper_args.add()
    p.key = 'testMountPoint'
    p.value = mount_point
    p = repl.helper_args.add()
    p.key = 'uid'
    p.value = '-1'
    p = repl.helper_args.add()
    p.key = 'gid'
    p.value = '-1'
    p = repl.helper_args.add()
    p.key = 'timeout'
    p.value = '1'

    return repl


def prepare_refresh_posix_helper_params(storage_id, mount_point):
    repl = fuse_messages_pb2.HelperParams()
    repl.helper_name = 'posix'
    p = repl.helper_args.add()
    p.key = 'type'
    p.value = 'posix'
    p = repl.helper_args.add()
    p.key = 'mountPoint'
    p.value = mount_point
    p = repl.helper_args.add()
    p.key = 'uid'
    p.value = '-1'
    p = repl.helper_args.add()
    p.key = 'gid'
    p.value = '-1'
    p = repl.helper_args.add()
    p.key = 'timeout'
    p.value = '1'

    return repl

def prepare_proxy_helper_response(storage_id):
    helper_params = fuse_messages_pb2.HelperParams()
    helper_params.helper_name = 'proxy'
    p = helper_params.helper_args.add()
    p.key = 'type'
    p.value = 'proxy'
    p = helper_params.helper_args.add()
    p.key = 'storageId'
    p.value = storage_id
    p = helper_params.helper_args.add()
    p.key = 'timeout'
    p.value = '1'


    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.helper_params.CopyFrom(helper_params)

    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_posix_helper_params_response(storage_id, mount_point):
    helper_params = prepare_posix_helper_params(storage_id, mount_point)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.helper_params.CopyFrom(helper_params)

    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_refresh_posix_helper_params_response(storage_id, mount_point):
    helper_params = prepare_refresh_posix_helper_params(storage_id,
            mount_point)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.helper_params.CopyFrom(helper_params)

    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_posix_create_storage_test_file(file_uuid, storage_id,
        space_id, file_content, mount_point):

    helper_params = prepare_posix_helper_params(storage_id, mount_point)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.storage_test_file.helper_params.CopyFrom(helper_params)
    server_response.fuse_response.storage_test_file.space_id = space_id
    server_response.fuse_response.storage_test_file.file_id = file_uuid

    server_response.fuse_response.storage_test_file.file_content = file_content

    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def test_automatic_posix_helper_detection_should_work(endpoint, hc, storage_id,
        space_id, file_uuid, file_content):

    assert hc.is_directio_forced() is False
    assert hc.is_proxyio_forced() is False

    mount_point = tempfile.mkdtemp(suffix='helperscache')
    create_storage_file_response = \
            prepare_posix_create_storage_test_file(file_uuid, storage_id,
                    space_id, file_content, mount_point)

    # Write random content to file
    with open(os.path.join(mount_point, file_uuid), 'wb') as test_file:
        test_file.write(file_content)

    verify_storage_test_file_response = messages_pb2.ServerMessage()
    verify_storage_test_file_response.fuse_response.status.code = \
            common_messages_pb2.Status.ok

    create_storage_file_request = None
    verify_storage_file_request = None

    with reply(endpoint, [create_storage_file_response,
                          verify_storage_test_file_response]) as queue:
        res = hc.get(file_uuid, space_id, storage_id, False)
        assert res
        create_storage_file_request = queue.get()
        verify_storage_file_request = queue.get()

    assert create_storage_file_request.HasField('fuse_request')
    create_storage_test_file = \
            create_storage_file_request.fuse_request.create_storage_test_file
    assert create_storage_test_file.storage_id == storage_id
    assert create_storage_test_file.file_uuid == file_uuid

    assert verify_storage_file_request.HasField('fuse_request')
    verify_storage_test_file = \
            verify_storage_file_request.fuse_request.verify_storage_test_file

    assert verify_storage_test_file.storage_id == storage_id
    assert verify_storage_test_file.space_id == space_id
    assert verify_storage_test_file.file_id == file_uuid
    file_contents = verify_storage_test_file.file_content

    assert hc.get_access_type(storage_id) == "direct"

    # Verify content written to the test file by storage access manager
    with open(os.path.join(mount_point, file_uuid), 'rb') as test_file:
        assert test_file.read() == file_contents


def test_directio_posix_helper_detection_should_work(endpoint, hc_directio,
        storage_id, space_id, file_uuid, file_content):

    assert hc_directio.is_directio_forced() is True

    mount_point = tempfile.mkdtemp(suffix='helperscache')

    posix_helper_params = \
            prepare_posix_helper_params_response(storage_id, mount_point)

    create_storage_file_response = \
            prepare_posix_create_storage_test_file(file_uuid, storage_id,
                    space_id, file_content, mount_point)

    # Write random content to file
    with open(os.path.join(mount_point, file_uuid), 'wb') as test_file:
        test_file.write(file_content)

    verify_storage_test_file_response = messages_pb2.ServerMessage()
    verify_storage_test_file_response.fuse_response.status.code = \
            common_messages_pb2.Status.ok

    get_helper_params_request = None
    create_storage_file_request = None
    verify_storage_file_request = None

    with reply(endpoint, [posix_helper_params,
                          create_storage_file_response,
                          verify_storage_test_file_response]) as queue:
        res = hc_directio.get(file_uuid, space_id, storage_id, False)
        assert res
        get_helper_params_request = queue.get()
        create_storage_file_request = queue.get()
        verify_storage_file_request = queue.get()

    assert get_helper_params_request.HasField('fuse_request')
    get_helper_params = \
            get_helper_params_request.fuse_request.get_helper_params
    assert get_helper_params.storage_id == storage_id
    assert get_helper_params.space_id == space_id
    assert get_helper_params.helper_mode == 3

    assert create_storage_file_request.HasField('fuse_request')
    create_storage_test_file = \
            create_storage_file_request.fuse_request.create_storage_test_file
    assert create_storage_test_file.storage_id == storage_id
    assert create_storage_test_file.file_uuid == file_uuid

    assert verify_storage_file_request.HasField('fuse_request')
    verify_storage_test_file = \
            verify_storage_file_request.fuse_request.verify_storage_test_file

    assert verify_storage_test_file.storage_id == storage_id
    assert verify_storage_test_file.space_id == space_id
    assert verify_storage_test_file.file_id == file_uuid
    file_contents = verify_storage_test_file.file_content

    assert hc_directio.get_access_type(storage_id) == "direct"

    # Verify content written to the test file by storage access manager
    with open(os.path.join(mount_point, file_uuid), 'rb') as test_file:
        assert test_file.read() == file_contents


def test_proxyio_posix_helper_detection_should_work(endpoint, hc_proxyio,
        storage_id, space_id, file_uuid):

    assert hc_proxyio.is_proxyio_forced() is True

    proxy_helper_params_response = prepare_proxy_helper_response(storage_id)

    proxy_helper_params_request = None

    with reply(endpoint, [proxy_helper_params_response]) as queue:
        res = hc_proxyio.get(file_uuid, space_id, storage_id, True)
        assert res
        proxy_helper_params_request = queue.get()

    assert proxy_helper_params_request.HasField('fuse_request')
    get_helper_params = proxy_helper_params_request.fuse_request.get_helper_params
    assert get_helper_params.storage_id == storage_id
    assert get_helper_params.space_id == space_id
    assert get_helper_params.helper_mode == 2

    assert hc_proxyio.get_access_type(storage_id) == "proxy"


def test_automatic_posix_helper_detection_should_fallback_to_proxy(endpoint,
           hc, storage_id, space_id, file_uuid, file_content):

    assert hc.is_directio_forced() is False
    assert hc.is_proxyio_forced() is False

    mount_point = tempfile.mkdtemp(suffix='helperscache')
    mount_point_invalid = tempfile.mkdtemp(suffix='helperscache')

    create_storage_file_response = \
        prepare_posix_create_storage_test_file(file_uuid, storage_id,
            space_id, file_content, mount_point_invalid)

    # Write random content to file
    with open(os.path.join(mount_point, file_uuid), 'wb') as test_file:
        test_file.write(file_content)

    proxy_helper_params_response = prepare_proxy_helper_response(storage_id)

    verify_storage_test_file_response = messages_pb2.ServerMessage()
    verify_storage_test_file_response.fuse_response.status.code = \
        common_messages_pb2.Status.ok

    create_storage_file_request = None
    proxy_helper_params_request = None

    with reply(endpoint, [create_storage_file_response,
                          proxy_helper_params_response,
                          create_storage_file_response]) as queue:
        res = hc.get(file_uuid, space_id, storage_id, False)
        create_storage_file_request = queue.get()
        proxy_helper_params_request = queue.get()
        create_storage_file_request = queue.get()

    # Wait for storage detection to complete in background
    time.sleep(15)

    assert create_storage_file_request.HasField('fuse_request')
    create_storage_test_file = \
        create_storage_file_request.fuse_request.create_storage_test_file
    assert create_storage_test_file.storage_id == storage_id
    assert create_storage_test_file.file_uuid == file_uuid

    assert proxy_helper_params_request.HasField('fuse_request')
    get_helper_params = \
        proxy_helper_params_request.fuse_request.get_helper_params
    assert get_helper_params.storage_id == storage_id
    assert get_helper_params.space_id == space_id
    assert get_helper_params.helper_mode == 2

    assert hc.get_access_type(storage_id) == "proxy"


def test_directio_posix_helper_detection_should_not_allow_proxy(
        endpoint, hc_directio, storage_id, space_id, file_uuid):

    assert hc_directio.is_directio_forced() is True

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, []) as queue:
            hc_directio.get(file_uuid, space_id, storage_id, True)
            queue.get()

    assert 'Operation not supported' in str(excinfo.value)


def test_helper_should_refresh_parameters(
        endpoint, hc_directio, storage_id, space_id, file_uuid, file_content):

    assert hc_directio.is_directio_forced() is True

    mount_point = tempfile.mkdtemp(suffix='helperscache')

    posix_helper_params = \
            prepare_posix_helper_params_response(storage_id, mount_point)

    create_storage_file_response = \
            prepare_posix_create_storage_test_file(file_uuid, storage_id,
                    space_id, file_content, mount_point)

    # Write random content to file
    with open(os.path.join(mount_point, file_uuid), 'wb') as test_file:
        test_file.write(file_content)

    verify_storage_test_file_response = messages_pb2.ServerMessage()
    verify_storage_test_file_response.fuse_response.status.code = \
            common_messages_pb2.Status.ok

    get_helper_params_request = None
    create_storage_file_request = None
    verify_storage_file_request = None

    with reply(endpoint, [posix_helper_params,
                          create_storage_file_response,
                          verify_storage_test_file_response]) as queue:
        res = hc_directio.get(file_uuid, space_id, storage_id, False)
        assert res
        get_helper_params_request = queue.get()
        create_storage_file_request = queue.get()
        verify_storage_file_request = queue.get()

    assert create_storage_file_request.HasField('fuse_request')
    create_storage_test_file = \
            create_storage_file_request.fuse_request.create_storage_test_file
    assert create_storage_test_file.storage_id == storage_id
    assert create_storage_test_file.file_uuid == file_uuid

    assert verify_storage_file_request.HasField('fuse_request')
    verify_storage_test_file = \
            verify_storage_file_request.fuse_request.verify_storage_test_file

    assert verify_storage_test_file.storage_id == storage_id
    assert verify_storage_test_file.space_id == space_id
    assert verify_storage_test_file.file_id == file_uuid
    file_contents = verify_storage_test_file.file_content

    assert hc_directio.get_access_type(storage_id) == "direct"

    # Verify content written to the test file by storage access manager
    with open(os.path.join(mount_point, file_uuid), 'rb') as test_file:
        assert test_file.read() == file_contents

    mount_point = tempfile.mkdtemp(suffix='helperscache')

    posix_helper_params = \
            prepare_refresh_posix_helper_params_response(storage_id,
                    mount_point)

    get_helper_params_request = None

    with reply(endpoint, [posix_helper_params]) as queue:
        # Refresh helper parameters
        hc_directio.refresh_helper_parameters(storage_id, space_id)

        res = hc_directio.get(file_uuid, space_id, storage_id, False)
        assert res
        get_helper_params_request = queue.get()

    assert get_helper_params_request.HasField('fuse_request')
    get_helper_params = \
            get_helper_params_request.fuse_request.get_helper_params
    assert get_helper_params.storage_id == storage_id
    assert get_helper_params.space_id == space_id
    assert get_helper_params.helper_mode == 3

    assert hc_directio.get_access_type(storage_id) == "direct"

    # Write to a file in new mountpoint using refreshed helper
    hc_directio.mknod('file1', space_id, storage_id, False)
    handle = hc_directio.open('file1', space_id, storage_id, False)
    file_contents = random_str()
    handle.write(0, file_contents)

    # Verify content written to the file by helper
    with open(os.path.join(mount_point, 'file1'), 'rb') as test_file:
        assert test_file.read() == file_contents
