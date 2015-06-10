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
        'parameters': [msg_num_param(10), msg_size_param(100, 'B')],
        'configs': {
            'multiple_small_messages': {
                'description': 'Sends multiple small messages using connection '
                               'pool.',
                'parameters': [msg_num_param(1000000)]
            },
            'multiple_large_messages': {
                'description': 'Sends multiple large messages using connection '
                               'pool.',
                'parameters': [msg_num_param(10000), msg_size_param(1, 'MB')]
            }
        }
    })
    def test_cp_should_send_messages(self, parameters):
        """Sends multiple messages using connection pool and checks whether they
        have been received."""

        appmock_client.reset_tcp_server_history(self.ip)
        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msg = random_str(msg_size)

        send_time = Duration()
        for _ in xrange(msg_num):
            duration(send_time, cp.send, msg)

        duration(send_time, appmock_client.tcp_server_wait_for_messages,
                 self.ip, 5555, msg, msg_num, False, 30)

        return [
            send_time_param(send_time.ms()),
            msg_per_sek_param(msg_num, send_time.us())
        ]

    @performance({
        'parameters': [msg_num_param(10), msg_size_param(100, 'B')],
        'configs': {
            'multiple_small_messages': {
                'description': 'Receives multiple small messages using '
                               'connection pool.',
                'parameters': [msg_num_param(10000)]
            },
            'multiple_large_messages': {
                'description': 'Receives multiple large messages using '
                               'connection pool.',
                'parameters': [msg_size_param(1, 'MB')]
            }
        }
    })
    def test_cp_should_receive_messages(self, parameters):
        """Receives multiple messages using connection pool."""

        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msgs = [random_str(msg_size) for _ in xrange(msg_num)]

        recv_time = Duration()
        for msg in msgs:
            duration(recv_time, appmock_client.tcp_server_send, self.ip, 5555,
                     msg)

        recv = []
        for _ in msgs:
            recv.append(duration(recv_time, cp.popMessage))

        assert len(msgs) == len(recv)
        assert msgs.sort() == recv.sort()

        return [
            recv_time_param(recv_time.ms()),
            msg_per_sek_param(msg_num, recv_time.us())
        ]


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

        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, handshake, 1,
                                                    False, 5)

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
        'repeats': 10,
        'parameters': [msg_num_param(1)],
        'configs': {
            'multiple_messages': {
                'description': 'Sends multiple messages using connection.',
                'parameters': [msg_num_param(500)],
            }
        }
    })
    def test_send_should_send_messages(self, parameters):
        """Sends multiple messages using connection and checks whether they
        have been received."""

        appmock_client.reset_tcp_server_history(self.ip)
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg = random_str()
        msg2 = random_str()

        send_time = Duration()
        for _ in xrange(msg_num):
            duration(send_time, appmock_client.tcp_server_send, self.ip, 5555,
                     random_str())

            duration(send_time, conn.waitForReady)
            duration(send_time, conn.send, msg)

            duration(send_time, conn.waitForReady)
            duration(send_time, conn.send, msg2)

        duration(send_time, appmock_client.tcp_server_wait_for_messages,
                 self.ip, 5555, msg, msg_num, False, 5)
        duration(send_time, appmock_client.tcp_server_wait_for_messages,
                 self.ip, 5555, msg2, msg_num, False, 5)

        return [
            send_time_param(send_time.ms()),
            msg_per_sek_param(msg_num, send_time.us())
        ]

    @performance({
        'repeats': 10,
        'parameters': [msg_num_param(1)],
        'configs': {
            'multiple_messages': {
                'description': 'Receives multiple messages using connection.',
                'parameters': [msg_num_param(500)]
            }
        }
    })
    def test_connection_should_receive(self, parameters):
        """Receives multiple messages using connection."""

        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())
        conn.getHandshakeResponse()

        msg_num = parameters['msg_num'].value

        recv_time = Duration()
        for _ in xrange(msg_num):
            msg = random_str()
            duration(recv_time, appmock_client.tcp_server_send, self.ip, 5555,
                     msg)

            assert duration(recv_time, conn.waitForMessage)
            assert msg == duration(recv_time, conn.getMessage)

            msg2 = random_str()
            duration(recv_time, appmock_client.tcp_server_send, self.ip, 5555,
                     msg2)

            assert duration(recv_time, conn.waitForMessage)
            assert msg2 == duration(recv_time, conn.getMessage)

        return [
            recv_time_param(recv_time.ms()),
            msg_per_sek_param(msg_num, recv_time.us())
        ]
