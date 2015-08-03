from __future__ import print_function

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import time
from multiprocessing import Process, Queue

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
from performance import *

# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import fslogic
import appmock_client
# noinspection PyUnresolvedReferences
from proto import messages_pb2, fuse_messages_pb2, common_messages_pb2


def with_reply(ip, fuse_response, queue):
    [received_msg] = appmock_client.tcp_server_wait_for_any_messages(ip, 5555,
                                                                     return_history=True)

    appmock_client.reset_tcp_server_history(ip)

    client_message = messages_pb2.ClientMessage()
    client_message.ParseFromString(received_msg)

    server_message = messages_pb2.ServerMessage()
    server_message.fuse_response.CopyFrom(fuse_response)
    server_message.message_id = client_message.message_id.encode('utf-8')

    sent_msg = server_message.SerializeToString()
    appmock_client.tcp_server_send(ip, 5555, sent_msg)

    queue.put(received_msg)


# noinspection PyClassHasNoInit
class TestCommunicator:
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

    @performance(skip=True)
    def test_getattrs_should_get_attrs(self, parameters):
        fl = fslogic.FsLogicProxy(self.ip, 5555)

        reply = fuse_messages_pb2.FileAttr()
        reply.uuid = '1234567'
        reply.name = 'path'
        reply.mode = 0567
        reply.uid = 123
        reply.gid = 456
        reply.ctime = int(time.time()) - 300
        reply.atime = int(time.time()) - 200
        reply.mtime = int(time.time()) - 100
        reply.type = fuse_messages_pb2.REG
        reply.size = 5362

        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.file_attr.CopyFrom(reply)
        fuse_response.status.code = common_messages_pb2.Status.VOK

        queue = Queue()
        p = Process(target=with_reply, args=(self.ip, fuse_response, queue))
        p.start()

        stat = fslogic.Stat()
        assert fl.getattr('/random/path', stat) == 0

        received_msg = queue.get()
        p.join()

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('get_file_attr')

        get_file_attr = fuse_request.get_file_attr
        assert get_file_attr.entry_type == fuse_messages_pb2.PATH
        assert get_file_attr.entry == '/random/path'

        assert stat.atime == reply.atime
        assert stat.mtime == reply.mtime
        assert stat.ctime == reply.ctime
        assert stat.gid == reply.gid
        assert stat.uid == reply.uid
        assert stat.mode == reply.mode | fslogic.regularMode()
        assert stat.size == reply.size
