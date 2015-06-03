"""This module tests connection pool."""

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import time

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
from performance import *

# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import connection_pool
import appmock_client


# noinspection PyClassHasNoInit
class TestConnectionPool:
    @classmethod
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns='none', uid=common.generate_uid(),
                                config_path=os.path.join(script_dir,
                                                         'env.json'))

        [container] = cls.result['docker_ids']
        cls.ip = docker.inspect(container)['NetworkSettings']['IPAddress']. \
            encode('ascii')

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    @performance({
        'parameters': [
            Parameter('msg_num', 'Number of messages sent.', 10),
            Parameter('msg_size', 'Size of each sent message.', 100, 'B')
        ],
        'configs': {
            'multiple_small_messages': {
                'description': 'Sends multiple small messages using connection '
                               'pool.',
                'parameters': [
                    Parameter('msg_num', 'Number of messages sent.', 10000)
                ]
            },
            'multiple_large_messages': {
                'description': 'Sends multiple large messages using connection '
                               'pool.',
                'parameters': [
                    Parameter('msg_size', 'Size of each sent message.', 1, 'MB')
                ]
            }
        }
    })
    def test_cp_should_send_messages(self, parameters):
        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msgs = [random_str(msg_size) for _ in xrange(msg_num)]

        for msg in msgs:
            cp.send(msg)

        time.sleep(1)

        for msg in msgs:
            assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                                msg)

    @performance({
        'parameters': [
            Parameter('msg_num', 'Number of messages sent.', 10),
            Parameter('msg_size', 'Size of each sent message.', 100, 'B')
        ],
        'configs': {
            'multiple_small_messages': {
                'description': 'Receives multiple small messages using '
                               'connection pool.',
                'parameters': [
                    Parameter('msg_num', 'Number of messages sent.', 10000)
                ]
            },
            'multiple_large_messages': {
                'description': 'Receives multiple large messages using '
                               'connection pool.',
                'parameters': [
                    Parameter('msg_size', 'Size of each sent message.', 1, 'MB')
                ]
            }
        }
    })
    def test_cp_should_receive_messages(self, parameters):
        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msgs = [random_str(msg_size) for _ in xrange(msg_num)]

        for msg in msgs:
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        time.sleep(1)

        recv = []
        for _ in msgs:
            recv.append(cp.popMessage())

        assert len(msgs) == len(recv)
        assert msgs.sort() == recv.sort()


# noinspection PyClassHasNoInit
class TestConnection:
    @classmethod
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns='none', uid=common.generate_uid(),
                                config_path=os.path.join(script_dir,
                                                         'env.json'))

        [container] = cls.result['docker_ids']
        cls.ip = docker.inspect(container)['NetworkSettings']['IPAddress']. \
            encode('ascii')

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    @performance(skip=True)
    def test_connect_should_trigger_handshake(self, parameters):
        handshake = random_str()

        conn = connection_pool.ConnectionProxy(handshake, True)
        conn.connect(self.ip, 5555)

        time.sleep(0.5)

        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            handshake)

    @performance(skip=True)
    def test_connection_should_receive_handshake_response(self, parameters):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        response = random_str()
        appmock_client.tcp_server_send(self.ip, 5555, response)

        assert response == conn.getHandshakeResponse()

    @performance(skip=True)
    def test_connection_should_close_after_rejected_handshake_response(self,
                                                                       parameters):
        conn = connection_pool.ConnectionProxy(random_str(), False)
        conn.connect(self.ip, 5555)

        assert not conn.isClosed()

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        assert conn.waitForClosed()

    @performance(skip=True)
    def test_connection_is_ready_after_handshake_response(self, parameters):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        assert not conn.isReady()

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        assert conn.waitForReady()

    @performance(skip=True)
    def test_connection_should_become_ready_after_message_sent(self,
                                                               parameters):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        conn.waitForReady()
        conn.send(random_str())

        assert conn.waitForReady()

    @performance({
        'parameters': [
            Parameter('msg_num', 'Number of messages sent.', 1),
        ],
        'configs': {
            'multiple_messages': {
                'description': 'Sends multiple messages using connection.',
                'parameters': [
                    Parameter('msg_num', 'Number of messages sent.', 100),
                ]
            }
        }
    })
    def test_send_should_send_messages(self, parameters):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg = random_str()
        msg2 = random_str()

        for _ in xrange(msg_num):
            appmock_client.tcp_server_send(self.ip, 5555, random_str())

            conn.waitForReady()
            conn.send(msg)

            conn.waitForReady()
            conn.send(msg2)

        time.sleep(0.5)
        assert msg_num == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                                  msg)
        assert msg_num == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                                  msg2)

    @performance({
        'parameters': [
            Parameter('msg_num', 'Number of messages sent.', 1),
        ],
        'configs': {
            'multiple_messages': {
                'description': 'Receives multiple messages using connection.',
                'parameters': [
                    Parameter('msg_num', 'Number of messages sent.', 100),
                ]
            }
        }
    })
    def test_connection_should_receive(self, parameters):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())
        conn.getHandshakeResponse()

        msg_num = parameters['msg_num'].value

        for _ in xrange(msg_num):
            msg = random_str()
            appmock_client.tcp_server_send(self.ip, 5555, msg)

            assert conn.waitForMessage()
            assert msg == conn.getMessage()

            msg2 = random_str()
            appmock_client.tcp_server_send(self.ip, 5555, msg2)

            assert conn.waitForMessage()
            assert msg2 == conn.getMessage()
