"""This module tests ProxyIO helper."""

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

import pytest
from proto import messages_pb2, common_messages_pb2

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
from proxy_helper import ProxyHelperProxy


@pytest.fixture
def storage_id():
    return random_str()


@pytest.fixture
def parameters():
    return random_params()


@pytest.fixture
def file_id():
    return random_str()


@pytest.fixture
def endpoint(appmock_client):
    return appmock_client.tcp_endpoint(5555)


@pytest.fixture
def helper(storage_id, endpoint):
    return ProxyHelperProxy(storage_id, endpoint.ip, endpoint.port)


@pytest.fixture
def file_handle(helper, file_id, parameters, request):
    handle = helper.open(file_id, parameters)
    request.addfinalizer(lambda: helper.release(handle))
    return handle


def remote_data_msg(data):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = common_messages_pb2.Status.ok
    server_message.proxyio_response.remote_data.data = data
    return server_message


def remote_write_result_msg(wrote):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = common_messages_pb2.Status.ok
    server_message.proxyio_response.remote_write_result.wrote = wrote
    return server_message


def test_write_should_write_data(file_handle, file_id, parameters, storage_id,
                                 endpoint, helper):
    data = random_str()
    offset = random_int()
    server_message = remote_write_result_msg(len(data))

    with reply(endpoint, server_message) as queue:
        assert len(data) == helper.write(file_handle, data, offset)
        received = queue.get()

    assert received.HasField('proxyio_request')

    request = received.proxyio_request
    assert decode_params(request.parameters) == parameters
    assert request.storage_id == storage_id
    assert request.file_id == file_id

    assert request.HasField('remote_write')
    assert request.remote_write.byte_sequence[0].offset == offset
    assert request.remote_write.byte_sequence[0].data == data


def test_write_should_pass_write_errors(file_id, endpoint, helper, parameters):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = \
        common_messages_pb2.Status.eacces

    file_handle = helper.open(file_id, parameters)

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, server_message):
            helper.write(file_handle, random_str(), random_int())

    assert 'Permission denied' in str(excinfo.value)


def test_read_should_read_data(file_handle, file_id, parameters, storage_id,
                               endpoint, helper):
    data = random_str()
    offset = random_int()
    server_message = remote_data_msg(data)

    with reply(endpoint, server_message) as queue:
        assert data == helper.read(file_handle, offset, len(data))
        received = queue.get()

    assert received.HasField('proxyio_request')

    request = received.proxyio_request
    assert decode_params(request.parameters) == parameters
    assert request.storage_id == storage_id
    assert request.file_id == file_id

    assert request.HasField('remote_read')
    assert request.remote_read.offset == offset
    assert request.remote_read.size == len(data)


def test_read_should_pass_errors(file_handle, file_id, endpoint, helper,
                                 parameters):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = \
        common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, server_message):
            helper.read(file_handle, random_int(), random_int())

    assert 'Operation not permitted' in str(excinfo.value)
