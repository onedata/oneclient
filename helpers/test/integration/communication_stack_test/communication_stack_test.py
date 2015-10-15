"""This module tests communication stack."""

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
from performance import *

# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
import communication_stack
import appmock_client


# noinspection PyClassHasNoInit
class TestCommunicator:
    @classmethod
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns_server='none', uid=common.generate_uid(),
                                config_path=os.path.join(script_dir,
                                                         'env.json'))

        [container] = cls.result['docker_ids']
        cls.ip = docker.inspect(container)['NetworkSettings']['IPAddress']. \
            encode('ascii')

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    def setup_method(self, _):
        appmock_client.reset_tcp_server_history(self.ip)

    @performance({
        'parameters': [msg_num_param(1), msg_size_param(100, 'B')],
        'configs': {
            'multiple_small_messages': {
                'description': 'Sends multiple small messages using '
                               'communicator.',
                'parameters': [msg_num_param(1000000)]
            },
            'multiple_large_messages': {
                'description': 'Sends multiple large messages using '
                               'communicator.',
                'parameters': [msg_num_param(50), msg_size_param(1, 'MB')]
            }
        }
    })
    def test_send(self, parameters):
        """Sends multiple messages using communicator."""

        com = communication_stack.Communicator(3, self.ip, 5555)
        com.connect()

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msg = random_str(msg_size)

        send_time = Duration()
        for _ in xrange(msg_num):
            sent_bytes = duration(send_time, com.send, msg)

        duration(send_time,
                 appmock_client.tcp_server_wait_for_specific_messages,
                 self.ip, 5555, sent_bytes, msg_num, False, False, 600)

        return [
            send_time_param(send_time.ms()),
            mbps_param(msg_num, msg_size, send_time.us())
        ]

    @performance({
        'repeats': 10,
        'parameters': [msg_num_param(1), msg_size_param(100, 'B')],
        'configs': {
            'multiple_small_messages': {
                'description': 'Receives multiple small messages using '
                               'communicator.',
                'parameters': [msg_num_param(1000)]
            },
            'multiple_large_messages': {
                'description': 'Receives multiple large messages using '
                               'communicator.',
                'parameters': [msg_num_param(50), msg_size_param(1, 'MB')]
            }
        }
    })
    def test_communicate(self, parameters):
        """Sends multiple messages and receives replies using communicator."""

        com = communication_stack.Communicator(1, self.ip, 5555)
        com.connect()

        appmock_client.tcp_server_wait_for_connections(self.ip, 5555, 1)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msg = random_str(msg_size)

        communicate_time = Duration()
        for _ in xrange(msg_num):
            request = duration(communicate_time, com.communicate, msg)
            reply = communication_stack.prepareReply(request, msg)

            duration(communicate_time,
                     appmock_client.tcp_server_wait_for_specific_messages,
                     self.ip, 5555, request, 1, False, False, 10)

            duration(communicate_time, appmock_client.tcp_server_send, self.ip,
                     5555, reply, 1)

            assert reply == duration(communicate_time, com.communicateReceive)

        return [
            communicate_time_param(communicate_time.ms()),
            mbps_param(msg_num, msg_size, communicate_time.us())
        ]

    @performance(skip=True)
    def test_successful_handshake(self, parameters):
        com = communication_stack.Communicator(1, self.ip, 5555)
        handshake = com.setHandshake("handshake", False)
        com.connect()

        com.sendAsync("this is another request")

        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                             handshake)
        assert 1 == appmock_client.tcp_server_all_messages_count(self.ip,
                                                                 5555)

        reply = communication_stack.prepareReply(handshake, "handshakeReply")
        appmock_client.tcp_server_send(self.ip, 5555, reply)

        assert com.handshakeResponse() == reply
        appmock_client.tcp_server_wait_for_any_messages(self.ip, 5555,
                                                        msg_count=2)

    @performance(skip=True)
    def test_unsuccessful_handshake(self, parameters):
        com = communication_stack.Communicator(3, self.ip, 5555)
        handshake = com.setHandshake("anotherHanshake", True)
        com.connect()

        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                             handshake, 3)

        reply = communication_stack.prepareReply(handshake, "anotherHandshakeR")
        appmock_client.tcp_server_send(self.ip, 5555, reply)

        # The connections should now be recreated and another handshake sent
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                             handshake, 6)
