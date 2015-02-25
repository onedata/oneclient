import os
import sys
import time

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *

from environment import appmock, common, docker
import connection_pool
import appmock_client


class TestConnectionPool:
    @classmethod
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns='none', uid=common.generate_uid(),
                                config_path=os.path.join(script_dir,
                                                         'env.json'))
        # TODO remove this sleep when appmock start is verified with nagios
        time.sleep(30)

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    def test_communication(self):
        [container] = self.result['docker_ids']
        ip = docker.inspect(container)['NetworkSettings']['IPAddress'].encode(
            'ascii')

        CP = connection_pool.ConnectionPoolProxy(3, ip, 5555)
        CP.send('test')
        appmock_client.tcp_server_send(ip, 5555, 'message')

        time.sleep(0.5)

        assert 1 == appmock_client.tcp_server_message_count(ip, 5555, "test")
        assert 'message' == CP.popMessage()
