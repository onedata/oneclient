"""This module tests connection pool."""

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

    def setup_method(self, _):
        appmock_client.reset_tcp_server_history(self.ip)

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

        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        msg_num = parameters['msg_num'].value
        msg_size = parameters['msg_size'].value * translate_unit(
            parameters['msg_size'].unit)
        msg = random_str(msg_size)

        send_time = Duration()
        for _ in xrange(msg_num):
            duration(send_time, cp.send, msg)

        duration(send_time,
                 appmock_client.tcp_server_wait_for_specific_messages,
                 self.ip, 5555, msg, msg_num, False, False, 600)

        return [
            send_time_param(send_time.ms()),
            mbps_param(msg_num, msg_size, send_time.us())
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
                     msg, 1)

        recv = []
        for _ in msgs:
            recv.append(duration(recv_time, cp.popMessage))

        assert len(msgs) == len(recv)
        assert msgs.sort() == recv.sort()

        return [
            recv_time_param(recv_time.ms()),
            mbps_param(msg_num, msg_size, recv_time.us())
        ]
