from __future__ import print_function
import random

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import time
from threading import Thread
from Queue import Queue

import pytest

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


def with_reply_process(ip, fuse_responses, queue):
    for i, fuse_response in enumerate(fuse_responses):
        [received_msg] = \
            appmock_client.tcp_server_wait_for_any_messages(ip, 5555,
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


def with_reply(ip, fuse_responses, fun, *args):
    if not isinstance(fuse_responses, list):
        fuse_responses = [fuse_responses]

    queue = Queue()
    p = Thread(target=with_reply_process, args=(ip, fuse_responses, queue))
    p.start()

    try:
        ret = fun(*args)
        received = [queue.get() for _ in fuse_responses]
        return ret, received
    except:
        raise
    finally:
        p.join()


def prepare_getattr(filename, filetype, size=None):
    reply = fuse_messages_pb2.FileAttr()
    reply.uuid = str(random.randint(0, 1000000000))
    reply.name = filename
    reply.mode = random.randint(0, 1023)
    reply.uid = random.randint(0, 20000)
    reply.gid = random.randint(0, 20000)
    reply.mtime = int(time.time()) - random.randint(0, 1000000)
    reply.atime = reply.mtime - random.randint(0, 1000000)
    reply.ctime = reply.atime - random.randint(0, 1000000)
    reply.type = filetype
    reply.size = size if size else random.randint(0, 1000000000)

    fuse_response = fuse_messages_pb2.FuseResponse()
    fuse_response.file_attr.CopyFrom(reply)
    fuse_response.status.code = common_messages_pb2.Status.VOK

    return fuse_response


def prepare_helper():
    reply = fuse_messages_pb2.HelperParams()
    reply.helper_name = 'null'

    fuse_response = fuse_messages_pb2.FuseResponse()
    fuse_response.helper_params.CopyFrom(reply)
    fuse_response.status.code = common_messages_pb2.Status.VOK

    return fuse_response


def prepare_location(file_blocks=None):
    reply = fuse_messages_pb2.FileLocation()
    reply.uuid = 'uuid1'
    reply.storage_id = 'storage1'
    reply.file_id = 'file1'
    reply.provider_id = 'provider1'

    if file_blocks:
        reply.blocks.extend(file_blocks)

    fuse_response = fuse_messages_pb2.FuseResponse()
    fuse_response.file_location.CopyFrom(reply)
    fuse_response.status.code = common_messages_pb2.Status.VOK

    return fuse_response


def do_open(self, file_blocks=None, size=None):
    blocks = []
    if file_blocks:
        for offset, size in file_blocks:
            block = common_messages_pb2.FileBlock()
            block.offset = offset
            block.size = size
            blocks.append(block)

    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG,
                                       size=size)

    open_response = prepare_location(blocks)

    (ret, _) = with_reply(self.ip, [getattr_response, open_response],
                          self.fl.open, '/random/path', 0)

    assert ret >= 0


# noinspection PyClassHasNoInit
class TestFsLogic:
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
        self.fl = fslogic.FsLogicProxy(self.ip, 5555)

    def teardown_method(self, _):
        del self.fl

    @performance(skip=True)
    def test_getattrs_should_get_attrs(self, parameters):
        fuse_response = prepare_getattr('path', fuse_messages_pb2.REG)

        stat = fslogic.Stat()
        (ret, [received_msg]) = with_reply(self.ip, fuse_response,
                                           self.fl.getattr, '/random/path',
                                           stat)

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('get_file_attr')

        get_file_attr = fuse_request.get_file_attr
        assert get_file_attr.entry_type == fuse_messages_pb2.PATH
        assert get_file_attr.entry == '/random/path'

        reply = fuse_response.file_attr
        assert stat.atime == reply.atime
        assert stat.mtime == reply.mtime
        assert stat.ctime == reply.ctime
        assert stat.gid == reply.gid
        assert stat.uid == reply.uid
        assert stat.mode == reply.mode | fslogic.regularMode()
        assert stat.size == reply.size

    @performance(skip=True)
    def test_getattrs_should_pass_errors(self, parameters):
        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.status.code = common_messages_pb2.Status.VENOENT

        stat = fslogic.Stat()
        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, fuse_response, self.fl.getattr,
                       '/random/path', stat)

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_getattrs_should_cache_attrs(self, parameters):
        fuse_response = prepare_getattr('path', fuse_messages_pb2.REG)

        stat = fslogic.Stat()
        (ret, _) = with_reply(self.ip, fuse_response, self.fl.getattr,
                              '/random/path', stat)

        assert ret == 0

        new_stat = fslogic.Stat()
        assert self.fl.getattr('/random/path', new_stat) == 0
        assert stat == new_stat
        assert 1 == appmock_client.tcp_server_all_messages_count(self.ip, 5555)

    @performance(skip=True)
    def test_mkdir_should_mkdir(self, parameters):
        getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
        mkdir_response = fuse_messages_pb2.FuseResponse()
        mkdir_response.status.code = common_messages_pb2.Status.VOK

        (_, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                      mkdir_response],
                                            self.fl.mkdir, '/random/path',
                                            0123)

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('create_dir')

        create_dir = fuse_request.create_dir
        assert create_dir.parent_uuid == getattr_response.file_attr.uuid
        assert create_dir.name == 'path'
        assert create_dir.mode == 0123

    @performance(skip=True)
    def test_mkdir_should_error_on_parent_not_dir(self, parameters):
        getattr_response = prepare_getattr('random', fuse_messages_pb2.REG)
        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.mkdir, '/random/path', 0123)

        assert 'Not a directory' in str(excinfo.value)

    @performance(skip=True)
    def test_mkdir_should_pass_getattr_errors(self, parameters):
        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.status.code = common_messages_pb2.Status.VENOENT

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, fuse_response, self.fl.mkdir,
                       '/random/path', 0123)

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_mkdir_should_pass_mkdir_errors(self, parameters):
        getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
        mkdir_response = fuse_messages_pb2.FuseResponse()
        mkdir_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, mkdir_response],
                       self.fl.mkdir, '/random/path', 0123)

            assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_rmdir_should_rmdir(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        rmdir_response = fuse_messages_pb2.FuseResponse()
        rmdir_response.status.code = common_messages_pb2.Status.VOK

        (ret, [_, received_msg]) = with_reply(self.ip,
                                              [getattr_response,
                                               rmdir_response],
                                              self.fl.rmdir,
                                              '/random/path')

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('delete_file')

        delete_file = fuse_request.delete_file
        assert delete_file.uuid == getattr_response.file_attr.uuid

    @performance(skip=True)
    def test_rmdir_should_pass_getattr_errors(self, parameters):
        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.status.code = common_messages_pb2.Status.VENOENT

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, fuse_response, self.fl.rmdir,
                       '/random/path')

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_rmdir_should_pass_rmdir_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        mkdir_response = fuse_messages_pb2.FuseResponse()
        mkdir_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, mkdir_response],
                       self.fl.rmdir, '/random/path')

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_rename_should_rename(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        rename_response = fuse_messages_pb2.FuseResponse()
        rename_response.status.code = common_messages_pb2.Status.VOK

        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        rename_response],
                                              self.fl.rename,
                                              '/random/path',
                                              '/random/path2')

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('rename')

        rename = fuse_request.rename
        assert rename.uuid == getattr_response.file_attr.uuid
        assert rename.target_path == '/random/path2'

    @performance(skip=True)
    def test_rename_should_change_caches(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        rename_response = fuse_messages_pb2.FuseResponse()
        rename_response.status.code = common_messages_pb2.Status.VOK

        with_reply(self.ip, [getattr_response, rename_response],
                   self.fl.rename, '/random/path', '/random/path2')

        stat = fslogic.Stat()
        self.fl.getattr('/random/path2', stat)

        assert stat.size == getattr_response.file_attr.size
        assert 1 == appmock_client.tcp_server_all_messages_count(self.ip, 5555)
        appmock_client.reset_tcp_server_history(self.ip)

        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VENOENT

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response, self.fl.getattr,
                       '/random/path', stat)

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_rename_should_pass_getattr_errors(self, parameters):
        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.status.code = common_messages_pb2.Status.VENOENT

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, fuse_response, self.fl.rename,
                       '/random/path', 'dawaw')

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_rename_should_pass_rename_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        mkdir_response = fuse_messages_pb2.FuseResponse()
        mkdir_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, mkdir_response],
                       self.fl.rename, '/random/path', 'dawaw2')

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_chmod_should_change_mode(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        chmod_response = fuse_messages_pb2.FuseResponse()
        chmod_response.status.code = common_messages_pb2.Status.VOK

        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        chmod_response],
                                              self.fl.chmod, '/random/path',
                                              0123)

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('change_mode')

        change_mode = fuse_request.change_mode
        assert change_mode.uuid == getattr_response.file_attr.uuid
        assert change_mode.mode == 0123

    @performance(skip=True)
    def test_chmod_should_change_cached_mode(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)

        stat = fslogic.Stat()
        with_reply(self.ip, getattr_response, self.fl.getattr,
                   '/random/path', stat)

        assert stat.mode == getattr_response.file_attr.mode | \
                            fslogic.regularMode()
        assert 1 == appmock_client.tcp_server_all_messages_count(self.ip, 5555)
        appmock_client.reset_tcp_server_history(self.ip)

        chmod_response = fuse_messages_pb2.FuseResponse()
        chmod_response.status.code = common_messages_pb2.Status.VOK
        with_reply(self.ip, chmod_response, self.fl.chmod, '/random/path',
                   0356)

        stat = fslogic.Stat()
        self.fl.getattr('/random/path', stat)

        assert stat.mode == 0356 | fslogic.regularMode()

    @performance(skip=True)
    def test_chmod_should_pass_getattr_errors(self, parameters):
        fuse_response = fuse_messages_pb2.FuseResponse()
        fuse_response.status.code = common_messages_pb2.Status.VENOENT

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, fuse_response, self.fl.chmod,
                       '/random/path', 0312)

        assert 'No such file or directory' in str(excinfo.value)

    @performance(skip=True)
    def test_chmod_should_pass_chmod_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        chmod_response = fuse_messages_pb2.FuseResponse()
        chmod_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, chmod_response],
                       self.fl.chmod, '/random/path', 0123)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_utime_should_update_times(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        utime_response = fuse_messages_pb2.FuseResponse()
        utime_response.status.code = common_messages_pb2.Status.VOK

        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        utime_response],
                                              self.fl.utime, '/random/path')

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('update_times')

        update_times = fuse_request.update_times
        assert update_times.uuid == getattr_response.file_attr.uuid
        assert update_times.atime == update_times.mtime
        assert update_times.atime <= time.time()
        assert not update_times.HasField('ctime')

    @performance(skip=True)
    def test_utime_should_change_cached_times(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)

        stat = fslogic.Stat()
        with_reply(self.ip, getattr_response, self.fl.getattr,
                   '/random/path', stat)

        assert stat.atime == getattr_response.file_attr.atime
        assert stat.mtime == getattr_response.file_attr.mtime
        assert 1 == appmock_client.tcp_server_all_messages_count(self.ip, 5555)
        appmock_client.reset_tcp_server_history(self.ip)

        utime_response = fuse_messages_pb2.FuseResponse()
        utime_response.status.code = common_messages_pb2.Status.VOK
        with_reply(self.ip, utime_response, self.fl.utime, '/random/path')

        stat = fslogic.Stat()
        self.fl.getattr('/random/path', stat)

        assert stat.atime != getattr_response.file_attr.atime
        assert stat.mtime != getattr_response.file_attr.mtime

    @performance(skip=True)
    def test_utime_should_update_times_with_buf(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
        utime_response = fuse_messages_pb2.FuseResponse()
        utime_response.status.code = common_messages_pb2.Status.VOK

        ubuf = fslogic.Ubuf()
        ubuf.actime = 54321
        ubuf.modtime = 12345

        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        utime_response],
                                              self.fl.utime_buf, '/random/path',
                                              ubuf)

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('update_times')

        update_times = fuse_request.update_times
        assert update_times.uuid == getattr_response.file_attr.uuid
        assert update_times.atime == ubuf.actime
        assert update_times.mtime == ubuf.modtime
        assert not update_times.HasField('ctime')

    @performance(skip=True)
    def test_utime_should_pass_getattr_errors(self, parameters):
        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.utime, '/random/path')

        assert 'Operation not permitted' in str(excinfo.value)

        ubuf = fslogic.Ubuf()
        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.utime_buf, '/random/path', ubuf)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_utime_should_pass_utime_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        utime_response = fuse_messages_pb2.FuseResponse()
        utime_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, utime_response],
                       self.fl.utime, '/random/path')

        assert 'Operation not permitted' in str(excinfo.value)

        ubuf = fslogic.Ubuf()
        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, utime_response,
                       self.fl.utime_buf, '/random/path', ubuf)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_readdir_should_read_dir(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)

        file1 = common_messages_pb2.ChildLink()
        file1.uuid = "uuid1"
        file1.name = "file1"

        file2 = common_messages_pb2.ChildLink()
        file2.uuid = "uuid2"
        file2.name = "file2"

        reply = fuse_messages_pb2.FileChildren()
        reply.child_links.extend([file1, file2])

        readdir_response = fuse_messages_pb2.FuseResponse()
        readdir_response.file_children.CopyFrom(reply)
        readdir_response.status.code = common_messages_pb2.Status.VOK

        children = []
        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        readdir_response],
                                              self.fl.readdir, '/random/path',
                                              children)

        assert ret == 0
        assert len(children) == 2
        assert file1.name in children
        assert file2.name in children

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('get_file_children')

        get_file_children = fuse_request.get_file_children
        assert get_file_children.uuid == getattr_response.file_attr.uuid
        assert get_file_children.offset == 0

    @performance(skip=True)
    def test_readdir_should_pass_getattr_errors(self, parameters):
        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            l = []
            with_reply(self.ip, getattr_response,
                       self.fl.readdir, '/random/path', l)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_readdir_should_pass_readdir_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
        readdir_response = fuse_messages_pb2.FuseResponse()
        readdir_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            l = []
            with_reply(self.ip, [getattr_response, readdir_response],
                       self.fl.readdir, '/random/path', l)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_mknod_should_make_new_location(self, parameters):
        getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
        mknod_response = prepare_location()

        (ret, [_, received_msg]) = with_reply(self.ip, [getattr_response,
                                                        mknod_response],
                                              self.fl.mknod, '/random/path',
                                              0762, 0)

        assert ret == 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('get_new_file_location')

        get_new_file_location = fuse_request.get_new_file_location
        assert get_new_file_location.parent_uuid == \
               getattr_response.file_attr.uuid
        assert get_new_file_location.name == 'path'
        assert get_new_file_location.mode == 0762

    @performance(skip=True)
    def test_mknod_should_pass_getattr_errors(self, parameters):
        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.mknod, '/random/path', 0724, 0)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_mknod_should_pass_location_errors(self, parameters):
        getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
        location_response = fuse_messages_pb2.FuseResponse()
        location_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, location_response],
                       self.fl.mknod, '/random/path', 0123, 0)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_open_should_get_file_location(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
        open_response = prepare_location()

        (ret, [_, received_msg]) = with_reply(self.ip,
                                              [getattr_response, open_response],
                                              self.fl.open, '/random/path', 0)

        assert ret >= 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('get_file_location')

        get_file_location = fuse_request.get_file_location
        assert get_file_location.uuid == getattr_response.file_attr.uuid

    @performance(skip=True)
    def test_open_should_pass_getattr_errors(self, parameters):
        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.open, '/random/path', 0)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_open_should_pass_location_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
        location_response = fuse_messages_pb2.FuseResponse()
        location_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, location_response],
                       self.fl.open, '/random/path', 0)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_read_should_read(self, parameters):
        do_open(self, file_blocks=[(0, 10)])
        assert 5 == self.fl.read('/random/path', 0, 5)

    @performance(skip=True)
    def test_read_should_read_zero_on_eof(self, parameters):
        do_open(self, file_blocks=[(0, 10)], size=10)
        assert 10 == self.fl.read('/random/path', 0, 12)
        assert 0 == self.fl.read('/random/path', 10, 2)

    @performance(skip=True)
    def test_read_should_pass_helper_errors(self, parameters):
        do_open(self, file_blocks=[(0, 10)], size=10)

        with pytest.raises(RuntimeError) as excinfo:
            self.fl.failHelper()
            self.fl.read('/random/path', 0, 10)

        assert 'Owner died' in str(excinfo.value)

    @performance(skip=True)
    def test_write_should_write(self, parameters):
        do_open(self, file_blocks=[(0, 10)], size=10)
        assert 5 == self.fl.write('/random/path', 0, 5)

    @performance(skip=True)
    def test_write_should_partition_writes(self, parameters):
        do_open(self, file_blocks=[(0, 5)], size=5)
        assert 5 == self.fl.write('/random/path', 0, 11)
        assert 6 == self.fl.write('/random/path', 5, 6)

    @performance(skip=True)
    def test_write_should_change_file_size(self, parameters):
        do_open(self, file_blocks=[(0, 5)], size=5)
        assert 20 == self.fl.write('/random/path', 10, 20)

        stat = fslogic.Stat()
        self.fl.getattr('/random/path', stat)
        assert 30 == stat.size

    @performance(skip=True)
    def test_write_should_pass_helper_errors(self, parameters):
        do_open(self, file_blocks=[(0, 10)], size=10)

        with pytest.raises(RuntimeError) as excinfo:
            self.fl.failHelper()
            self.fl.write('/random/path', 0, 10)

        assert 'Owner died' in str(excinfo.value)

    @performance(skip=True)
    def test_truncate_should_truncate(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
        truncate_response = fuse_messages_pb2.FuseResponse()
        truncate_response.status.code = common_messages_pb2.Status.VOK
        location_response = prepare_location()

        (ret, [_, received_msg, _]) = with_reply(
            self.ip, [getattr_response, truncate_response, location_response],
            self.fl.truncate, '/random/path', 4)

        assert ret >= 0

        client_message = messages_pb2.ClientMessage()
        client_message.ParseFromString(received_msg)

        assert client_message.HasField('fuse_request')

        fuse_request = client_message.fuse_request
        assert fuse_request.HasField('truncate')

        truncate = fuse_request.truncate
        assert truncate.uuid == getattr_response.file_attr.uuid
        assert truncate.size == 4

    @performance(skip=True)
    def test_truncate_should_pass_getattr_errors(self, parameters):
        getattr_response = fuse_messages_pb2.FuseResponse()
        getattr_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, getattr_response,
                       self.fl.truncate, '/random/path', 2)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_truncate_should_pass_truncate_errors(self, parameters):
        getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
        truncate_response = fuse_messages_pb2.FuseResponse()
        truncate_response.status.code = common_messages_pb2.Status.VEPERM

        with pytest.raises(RuntimeError) as excinfo:
            with_reply(self.ip, [getattr_response, truncate_response],
                       self.fl.truncate, '/random/path', 3)

        assert 'Operation not permitted' in str(excinfo.value)

    @performance(skip=True)
    def test_readdir_big_directory(self, parameters):
        children_num = 100000

        getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)

        reply = fuse_messages_pb2.FileChildren()
        for i in xrange(0, children_num):
            link = reply.child_links.add()
            link.uuid = "uuid{0}".format(i)
            link.name = "file{0}".format(i)

        readdir_response = fuse_messages_pb2.FuseResponse()
        readdir_response.file_children.CopyFrom(reply)
        readdir_response.status.code = common_messages_pb2.Status.VOK

        children = []
        (ret, _) = with_reply(self.ip, [getattr_response, readdir_response],
                              self.fl.readdir, '/random/path', children)

        assert ret == 0
        assert len(children) == children_num

    @performance(skip=True)
    def test_write_should_save_blocks(self, parameters):
        do_open(self, size=0)
        assert 5 == self.fl.write('/random/path', 0, 5)
        assert 5 == self.fl.read('/random/path', 0, 10)
