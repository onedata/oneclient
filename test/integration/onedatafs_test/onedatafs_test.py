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
import pytest
from stat import *

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import onedatafs
# noinspection PyUnresolvedReferences
from proto import messages_pb2, fuse_messages_pb2, event_messages_pb2, \
    common_messages_pb2, stream_messages_pb2, diagnostic_messages_pb2, \
    handshake_messages_pb2


@pytest.yield_fixture(scope="function")
def endpoint(appmock_client):
    app = appmock_client.tcp_endpoint(443)
    yield app
    appmock_client.reset_tcp_history()


@pytest.fixture
def uuid():
    return 'rootUuid'

@pytest.fixture
def token():
    return 'MDAzOWxvY2F00aW9uIGxkLWRldi1vbmV6b25lLmxkLWRldi5zdmMuZGV2Lm9uZWRhdGEudWsudG8KMDAzMGlkZW500aWZpZXIgYzE5MDg2Yzg5MjIzMzZjMGIzMzQzMzRjNmMxMTMyNWEKMDAxYWNpZCB00aW1lIDwgMTYwODE5NzMxMQowMDJmc2lnbmF00dXJlIL2k600mP9I01wKZgHrBCCSF4TahY19v9THsbi77tTc9RyCg'


def prepare_handshake_response(status):
    repl = handshake_messages_pb2.HandshakeResponse()
    repl.status = status

    response = messages_pb2.ServerMessage()
    response.handshake_response.CopyFrom(repl)

    return response


def prepare_configuration_response(root_uuid):
    repl = diagnostic_messages_pb2.Configuration()
    repl.root_uuid = root_uuid

    response = messages_pb2.ServerMessage()
    response.configuration.CopyFrom(repl)

    return response

@pytest.mark.skip()
def test_onedatafs_should_connect_to_provider(endpoint, uuid, token):
    handshake_response = prepare_handshake_response(1)

    response_ok = messages_pb2.ServerMessage()
    response_ok.fuse_response.status.code = common_messages_pb2.Status.ok

    configuration_response = prepare_configuration_response(uuid)

    odfs = None
    with reply(endpoint, [handshake_response,
                          response_ok, # Match to stream_reset
                          configuration_response,
                          handshake_response, response_ok], True) as queue:
        odfs = onedatafs.OnedataFS(
            endpoint.ip,
            token,
            port=endpoint.port,
            insecure=True,
            log_level=-1,
            provider_timeout=5,
            cli_args="--communicator-pool-size 1")

    assert uuid == odfs.root_uuid()

    odfs.close()


def test_onedatafs_should_raise_exception_on_bad_token(endpoint, uuid, token):
    handshake_response = prepare_handshake_response(2)

    response_ok = messages_pb2.ServerMessage()
    response_ok.fuse_response.status.code = common_messages_pb2.Status.ok

    configuration_response = prepare_configuration_response(uuid)

    odfs = None
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [handshake_response,
                            response_ok, # Match to stream_reset
                            configuration_response,
                            handshake_response], True) as queue:
            odfs = onedatafs.OnedataFS(
                endpoint.ip,
                token,
                port=endpoint.port,
                insecure=True,
                log_level=-1,
                provider_timeout=5,
                cli_args="--communicator-pool-size 1")

    assert "Authentication to Oneprovider failed" in str(excinfo.value)
