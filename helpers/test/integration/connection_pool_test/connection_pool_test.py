import os
import sys
import time

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *

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

    def test_cp_should_send_messages(self):
        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        messages = [random_str() for _ in range(10)]

        for msg in messages:
            cp.send(msg)

        time.sleep(1)

        for msg in messages:
            assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                                msg)

    def test_cp_should_receive_messages(self):
        cp = connection_pool.ConnectionPoolProxy(3, self.ip, 5555)

        messages = [random_str() for _ in range(10)]

        for msg in messages:
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        time.sleep(1)

        received = []
        for _ in messages:
            received.append(cp.popMessage())

        assert len(messages) == len(received)
        assert messages.sort() == received.sort()


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

    def test_connect_should_trigger_handshake(self):
        handshake = random_str()

        conn = connection_pool.ConnectionProxy(handshake, True)
        conn.connect(self.ip, 5555)

        time.sleep(0.5)

        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555,
                                                            handshake)

    def test_connection_should_receive_handshake_response(self):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        response = random_str()
        appmock_client.tcp_server_send(self.ip, 5555, response)

        assert response == conn.getHandshakeResponse()

    def test_connection_should_close_after_rejected_handshake_response(self):
        conn = connection_pool.ConnectionProxy(random_str(), False)
        conn.connect(self.ip, 5555)

        assert not conn.isClosed()

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        assert conn.waitForClosed()

    def test_connection_is_ready_after_handshake_response(self):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        assert not conn.isReady()

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        assert conn.waitForReady()

    def test_connection_should_become_ready_after_message_sent(self):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        conn.waitForReady()
        conn.send(random_str())

        assert conn.waitForReady()

    def test_send_should_send_messages(self):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())

        conn.waitForReady()

        msg = random_str()
        conn.send(msg)

        conn.waitForReady()

        msg2 = random_str()
        conn.send(msg2)

        time.sleep(0.5)
        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555, msg)
        assert 1 == appmock_client.tcp_server_message_count(self.ip, 5555, msg2)

    def test_connection_should_receive(self):
        conn = connection_pool.ConnectionProxy(random_str(), True)
        conn.connect(self.ip, 5555)

        appmock_client.tcp_server_send(self.ip, 5555, random_str())
        conn.getHandshakeResponse()

        msg = random_str()
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        assert conn.waitForMessage()
        assert msg == conn.getMessage()

        msg2 = random_str()
        appmock_client.tcp_server_send(self.ip, 5555, msg2)

        assert conn.waitForMessage()
        assert msg2 == conn.getMessage()
