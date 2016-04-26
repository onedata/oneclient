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
import proxy_io


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
    return proxy_io.ProxyIOProxy(storage_id, endpoint.ip,
                                 endpoint.port)


@pytest.fixture
def file_ctx(helper, file_id, parameters):
    return helper.open(file_id, parameters)


def test_write_should_write_data(file_ctx, file_id, parameters, storage_id, endpoint, helper):
    wrote = random_int()
    data = random_str()
    offset = random_int()

    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = common_messages_pb2.Status.ok
    server_message.proxyio_response.remote_write_result.wrote = wrote

    with reply(endpoint, server_message) as queue:
        assert wrote == helper.write(file_ctx, file_id, data, offset)
        received = queue.get()

    assert received.HasField('proxyio_request')

    request = received.proxyio_request
    assert decode_params(request.parameters) == parameters
    assert request.storage_id == storage_id
    assert request.file_id == file_id

    assert request.HasField('remote_write')
    assert request.remote_write.offset == offset
    assert request.remote_write.data == data


def test_write_should_pass_errors(file_ctx, endpoint, helper):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = \
        common_messages_pb2.Status.eacces

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, server_message):
            helper.write(file_ctx, random_str(), random_str(), random_int())

    assert 'Permission denied' in str(excinfo.value)


def test_read_should_read_data(file_ctx, file_id, parameters, storage_id, endpoint, helper):
    data = random_str()
    offset = random_int()

    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = common_messages_pb2.Status.ok
    server_message.proxyio_response.remote_data.data = data

    with reply(endpoint, server_message) as queue:
        assert data == helper.read(file_ctx, file_id, offset, len(data))
        received = queue.get()

    assert received.HasField('proxyio_request')

    request = received.proxyio_request
    assert decode_params(request.parameters) == parameters
    assert request.storage_id == storage_id
    assert request.file_id == file_id

    assert request.HasField('remote_read')
    assert request.remote_read.offset == offset
    assert request.remote_read.size == len(data)


def test_read_should_pass_errors(file_ctx, endpoint, helper):
    server_message = messages_pb2.ServerMessage()
    server_message.proxyio_response.status.code = \
        common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, server_message):
            helper.read(file_ctx, random_str(), random_int(), random_int())

    assert 'Operation not permitted' in str(excinfo.value)
