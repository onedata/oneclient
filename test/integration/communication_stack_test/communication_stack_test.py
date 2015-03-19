import os
import sys
import time

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *

from environment import appmock, common, docker
import communication_stack
import appmock_client


class TestCommunicator:
    @classmethod
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns='none', uid=common.generate_uid(),
                                config_path=os.path.join(script_dir,
                                                         'env.json'))
        # TODO remove this sleep when appmock start is verified with nagios
        time.sleep(30)

        [container] = cls.result['docker_ids']
        cls.ip = docker.inspect(container)['NetworkSettings']['IPAddress']. \
            encode('ascii')

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    def test_send(self):
        com = communication_stack.Communicator(3, self.ip, 5555)
        com.connect()

        sent_bytes = com.send("this is a message")

        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            sent_bytes)

    def test_communicate(self):
        com = communication_stack.Communicator(1, self.ip, 5555)
        com.connect()

        request = com.communicate("this is a request")

        reply = communication_stack.prepareReply(request, "this is a reply")
        time.sleep(0.5)

        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            request)
        appmock_client.tcp_server_send(self.ip, 5555, reply)

        assert com.communicateReceive() == reply

    def test_successful_handshake(self):
        com = communication_stack.Communicator(1, self.ip, 5555)
        handshake = com.setHandshake("handshake", False)
        com.connect()

        request = com.send("this is another request")

        time.sleep(0.5)
        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            handshake)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            request)

        reply = communication_stack.prepareReply(handshake, "handshakeReply")
        appmock_client.tcp_server_send(self.ip, 5555, reply)

        assert com.handshakeResponse() == reply
        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            request)
