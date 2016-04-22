from __future__ import print_function

import random

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import time
import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import fslogic
# noinspection PyUnresolvedReferences
from proto import messages_pb2, fuse_messages_pb2, event_messages_pb2, \
    common_messages_pb2, stream_messages_pb2


@pytest.fixture
def endpoint(appmock_client):
    return appmock_client.tcp_endpoint(5555)


@pytest.fixture
def fl(endpoint):
    return fslogic.FsLogicProxy(endpoint.ip, endpoint.port)


def prepare_file_blocks(blocks=[]):
    file_blocks = []
    for file_block in blocks:
        block = common_messages_pb2.FileBlock()
        if len(file_block) == 2:
            offset, block_size = file_block
        else:
            offset, block_size, storage_id, file_id = file_block
            block.storage_id = storage_id
            block.file_id = file_id
        block.offset = offset
        block.size = block_size
        file_blocks.append(block)
    return file_blocks


def prepare_location_update_event(blocks, stream_id, sequence_number):
    file_blocks = prepare_file_blocks(blocks)

    file_location = fuse_messages_pb2.FileLocation()
    file_location.uuid = 'uuid1'
    file_location.space_id = 'space1'
    file_location.storage_id = 'storage1'
    file_location.file_id = 'file1'
    file_location.provider_id = 'provider1'
    file_location.blocks.extend(file_blocks)

    update_event = event_messages_pb2.UpdateEvent()
    update_event.file_location.CopyFrom(file_location)

    event = event_messages_pb2.Event()
    event.counter = 1
    event.update_event.CopyFrom(update_event)

    events = event_messages_pb2.Events()
    events.events.extend([event])

    message_stream = stream_messages_pb2.MessageStream()
    message_stream.stream_id = stream_id
    message_stream.sequence_number = sequence_number

    server_msg = messages_pb2.ServerMessage()
    server_msg.message_stream.CopyFrom(message_stream)
    server_msg.events.CopyFrom(events)

    return server_msg


def prepare_synchronize_block(offset, size):
    block = common_messages_pb2.FileBlock()
    block.offset = offset
    block.size = size

    req = fuse_messages_pb2.SynchronizeBlock()
    req.uuid = 'uuid1'
    req.block.CopyFrom(block)

    client_request = messages_pb2.ClientMessage()
    client_request.fuse_request.synchronize_block.CopyFrom(req)

    return client_request


def prepare_getattr(filename, filetype, size=None):
    repl = fuse_messages_pb2.FileAttr()
    repl.uuid = 'uuid1'
    repl.name = filename
    repl.mode = random.randint(0, 1023)
    repl.uid = random.randint(0, 20000)
    repl.gid = random.randint(0, 20000)
    repl.mtime = int(time.time()) - random.randint(0, 1000000)
    repl.atime = repl.mtime - random.randint(0, 1000000)
    repl.ctime = repl.atime - random.randint(0, 1000000)
    repl.type = filetype
    repl.size = size if size else random.randint(0, 1000000000)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_attr.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_helper():
    repl = fuse_messages_pb2.HelperParams()
    repl.helper_name = 'null'

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.helper_params.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_location(blocks=[], handle_id=None):
    file_blocks = prepare_file_blocks(blocks)

    repl = fuse_messages_pb2.FileLocation()
    repl.uuid = 'uuid1'
    repl.space_id = 'space1'
    repl.storage_id = 'storage1'
    repl.file_id = 'file1'
    repl.provider_id = 'provider1'
    repl.blocks.extend(file_blocks)
    if handle_id is not None:
        repl.handle_id = handle_id

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_location.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def do_open(endpoint, fl, blocks=[], size=None, handle_id=None):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG,
                                       size=size)

    open_response = prepare_location(blocks, handle_id)

    with reply(endpoint, [getattr_response, open_response]):
        assert fl.open('/random/path', 0) >= 0

def do_read(fl, path, offset, size):
    fl.read(path, offset, size)

def get_stream_id_from_location_subscription(subscription_message_data):
    location_subsc = messages_pb2.ClientMessage()
    location_subsc.ParseFromString(subscription_message_data)
    return location_subsc.message_stream.stream_id

def test_getattrs_should_get_attrs(endpoint, fl):
    response = prepare_getattr('path', fuse_messages_pb2.REG)

    stat = fslogic.Stat()
    with reply(endpoint, response) as queue:
        assert 0 == fl.getattr('/random/path', stat)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('get_file_attr')

    get_file_attr = fuse_request.get_file_attr
    assert get_file_attr.entry_type == fuse_messages_pb2.PATH
    assert get_file_attr.entry == '/random/path'

    repl = response.fuse_response.file_attr
    assert stat.atime == repl.atime
    assert stat.mtime == repl.mtime
    assert stat.ctime == repl.ctime
    assert stat.gid == repl.gid
    assert stat.uid == repl.uid
    assert stat.mode == repl.mode | fslogic.regularMode()
    assert stat.size == repl.size


def test_getattrs_should_pass_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    stat = fslogic.Stat()
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr('/random/path', stat)

    assert 'No such file or directory' in str(excinfo.value)


def test_getattrs_should_cache_attrs(endpoint, fl):
    fuse_response = prepare_getattr('path', fuse_messages_pb2.REG)

    stat = fslogic.Stat()
    with reply(endpoint, fuse_response):
        assert 0 == fl.getattr('/random/path', stat)

    new_stat = fslogic.Stat()
    assert fl.getattr('/random/path', new_stat) == 0
    assert stat == new_stat
    assert 2 == endpoint.all_messages_count()


def test_mkdir_should_mkdir(endpoint, fl):
    getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        fl.mkdir('/random/path', 0123)
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('create_dir')

    create_dir = fuse_request.create_dir
    assert create_dir.name == 'path'
    assert create_dir.mode == 0123
    assert create_dir.parent_uuid == \
           getattr_response.fuse_response.file_attr.uuid


def test_mkdir_should_error_on_parent_not_dir(endpoint, fl):
    getattr_response = prepare_getattr('random', fuse_messages_pb2.REG)
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, getattr_response):
            fl.mkdir('/random/path', 0123)

    assert 'Not a directory' in str(excinfo.value)


def test_mkdir_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.mkdir('/random/path', 0123)

    assert 'No such file or directory' in str(excinfo.value)


def test_mkdir_should_pass_mkdir_errors(endpoint, fl):
    getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.mkdir('/random/path', 0123)

    assert 'Operation not permitted' in str(excinfo.value)


def test_rmdir_should_rmdir(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.rmdir('/random/path')
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('delete_file')

    delete_file = fuse_request.delete_file
    assert delete_file.uuid == getattr_response.fuse_response.file_attr.uuid


def test_rmdir_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.rmdir('/random/path')

    assert 'No such file or directory' in str(excinfo.value)


def test_rmdir_should_pass_rmdir_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rmdir('/random/path')

    assert 'Operation not permitted' in str(excinfo.value)


def test_rename_should_rename(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.rename('/random/path', '/random/path2')
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('rename')

    rename = fuse_request.rename
    assert rename.uuid == getattr_response.fuse_response.file_attr.uuid
    assert rename.target_path == '/random/path2'


def test_rename_should_change_caches(appmock_client, endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]):
        fl.rename('/random/path', '/random/path2')

    stat = fslogic.Stat()
    fl.getattr('/random/path2', stat)

    assert stat.size == getattr_response.fuse_response.file_attr.size
    1 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr('/random/path', stat)

    assert 'No such file or directory' in str(excinfo.value)


def test_rename_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.rename('/random/path', 'dawaw')

    assert 'No such file or directory' in str(excinfo.value)


def test_rename_should_pass_rename_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rename('/random/path', 'dawaw2')

    assert 'Operation not permitted' in str(excinfo.value)


def test_chmod_should_change_mode(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.chmod('/random/path', 0123)
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('change_mode')

    change_mode = fuse_request.change_mode
    assert change_mode.uuid == getattr_response.fuse_response.file_attr.uuid
    assert change_mode.mode == 0123


def test_chmod_should_change_cached_mode(appmock_client, endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)

    stat = fslogic.Stat()
    with reply(endpoint, getattr_response):
        fl.getattr('/random/path', stat)

    assert stat.mode == getattr_response.fuse_response.file_attr.mode | \
                        fslogic.regularMode()
    assert 2 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    with reply(endpoint, response):
        fl.chmod('/random/path', 0356)

    stat = fslogic.Stat()
    fl.getattr('/random/path', stat)

    assert stat.mode == 0356 | fslogic.regularMode()


def test_chmod_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.chmod('/random/path', 0312)

    assert 'No such file or directory' in str(excinfo.value)


def test_chmod_should_pass_chmod_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.chmod('/random/path', 0123)

    assert 'Operation not permitted' in str(excinfo.value)


def test_utime_should_update_times(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.utime('/random/path')
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('update_times')

    update_times = fuse_request.update_times
    assert update_times.uuid == getattr_response.fuse_response.file_attr.uuid
    assert update_times.atime == update_times.mtime
    assert update_times.atime <= time.time()
    assert not update_times.HasField('ctime')


def test_utime_should_change_cached_times(appmock_client, endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)

    stat = fslogic.Stat()
    with reply(endpoint, getattr_response):
        fl.getattr('/random/path', stat)

    assert stat.atime == getattr_response.fuse_response.file_attr.atime
    assert stat.mtime == getattr_response.fuse_response.file_attr.mtime
    assert 2 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    with reply(endpoint, response):
        fl.utime('/random/path')

    stat = fslogic.Stat()
    fl.getattr('/random/path', stat)

    assert stat.atime != getattr_response.fuse_response.file_attr.atime
    assert stat.mtime != getattr_response.fuse_response.file_attr.mtime


def test_utime_should_update_times_with_buf(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    ubuf = fslogic.Ubuf()
    ubuf.actime = 54321
    ubuf.modtime = 12345

    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.utime_buf('/random/path', ubuf)
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('update_times')

    update_times = fuse_request.update_times
    assert update_times.uuid == getattr_response.fuse_response.file_attr.uuid
    assert update_times.atime == ubuf.actime
    assert update_times.mtime == ubuf.modtime
    assert not update_times.HasField('ctime')


def test_utime_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.utime('/random/path')

    assert 'Operation not permitted' in str(excinfo.value)

    ubuf = fslogic.Ubuf()
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.utime_buf('/random/path', ubuf)

    assert 'Operation not permitted' in str(excinfo.value)


def test_utime_should_pass_utime_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.utime('/random/path')

    assert 'Operation not permitted' in str(excinfo.value)

    ubuf = fslogic.Ubuf()
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.utime_buf('/random/path', ubuf)

    assert 'Operation not permitted' in str(excinfo.value)


def test_readdir_should_read_dir(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)

    file1 = common_messages_pb2.ChildLink()
    file1.uuid = "uuid1"
    file1.name = "file1"

    file2 = common_messages_pb2.ChildLink()
    file2.uuid = "uuid2"
    file2.name = "file2"

    repl = fuse_messages_pb2.FileChildren()
    repl.child_links.extend([file1, file2])

    response = messages_pb2.ServerMessage()
    response.fuse_response.file_children.CopyFrom(repl)
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    with reply(endpoint, [getattr_response, response]) as queue:
        assert 0 == fl.readdir('/random/path', children)
        queue.get()
        client_message = queue.get()

    assert len(children) == 2
    assert file1.name in children
    assert file2.name in children

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('get_file_children')

    get_file_children = fuse_request.get_file_children
    assert get_file_children.offset == 0
    assert get_file_children.uuid == \
           getattr_response.fuse_response.file_attr.uuid


def test_readdir_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        l = []
        with reply(endpoint, response):
            fl.readdir('/random/path', l)

    assert 'Operation not permitted' in str(excinfo.value)


def test_readdir_should_pass_readdir_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        l = []
        with reply(endpoint, [getattr_response, response]):
            fl.readdir('/random/path', l)

    assert 'Operation not permitted' in str(excinfo.value)


def test_mknod_should_make_new_location(endpoint, fl):
    getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
    mknod_response = prepare_location()

    with reply(endpoint, [getattr_response, mknod_response]) as queue:
        assert 0 == fl.mknod('/random/path', 0762, 0)
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('get_new_file_location')

    get_new_file_location = fuse_request.get_new_file_location
    assert get_new_file_location.parent_uuid == \
           getattr_response.fuse_response.file_attr.uuid
    assert get_new_file_location.name == 'path'
    assert get_new_file_location.mode == 0762


def test_mknod_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.mknod('/random/path', 0724, 0)

    assert 'Operation not permitted' in str(excinfo.value)


def test_mknod_should_pass_location_errors(endpoint, fl):
    getattr_response = prepare_getattr('random', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.mknod('/random/path', 0123, 0)

    assert 'Operation not permitted' in str(excinfo.value)


def test_open_should_get_file_location(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
    open_response = prepare_location()

    with reply(endpoint, [getattr_response, open_response]) as queue:
        assert fl.open('/random/path', 0) >= 0
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('get_file_location')

    get_file_location = fuse_request.get_file_location
    assert get_file_location.uuid == \
           getattr_response.fuse_response.file_attr.uuid


def test_open_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.open('/random/path', 0)

    assert 'Operation not permitted' in str(excinfo.value)


def test_open_should_pass_location_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.open('/random/path', 0)

    assert 'Operation not permitted' in str(excinfo.value)


def test_read_should_read(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 10)])
    assert 5 == fl.read('/random/path', 0, 5)


def test_read_should_read_zero_on_eof(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 10)], size=10)
    assert 10 == fl.read('/random/path', 0, 12)
    assert 0 == fl.read('/random/path', 10, 2)


def test_read_should_pass_helper_errors(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 10)], size=10)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.read('/random/path', 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_write_should_write(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 10)], size=10)
    assert 5 == fl.write('/random/path', 0, 5)


def test_write_should_partition_writes(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5)], size=5)
    assert 5 == fl.write('/random/path', 0, 11)
    assert 6 == fl.write('/random/path', 5, 6)


def test_write_should_change_file_size(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5)], size=5)
    assert 20 == fl.write('/random/path', 10, 20)

    stat = fslogic.Stat()
    fl.getattr('/random/path', stat)
    assert 30 == stat.size


def test_write_should_pass_helper_errors(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 10)], size=10)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.write('/random/path', 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_truncate_should_truncate(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    loc_response = prepare_location()

    with reply(endpoint, [getattr_response, response, loc_response]) as queue:
        assert fl.truncate('/random/path', 4) >= 0
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('truncate')

    truncate = fuse_request.truncate
    assert truncate.uuid == getattr_response.fuse_response.file_attr.uuid
    assert truncate.size == 4


def test_truncate_should_pass_getattr_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.truncate('/random/path', 2)

    assert 'Operation not permitted' in str(excinfo.value)


def test_truncate_should_pass_truncate_errors(endpoint, fl):
    getattr_response = prepare_getattr('path', fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.truncate('/random/path', 3)

    assert 'Operation not permitted' in str(excinfo.value)


def test_readdir_big_directory(endpoint, fl):
    children_num = 100000

    getattr_response = prepare_getattr('path', fuse_messages_pb2.DIR)

    repl = fuse_messages_pb2.FileChildren()
    for i in xrange(0, children_num):
        link = repl.child_links.add()
        link.uuid = "uuid{0}".format(i)
        link.name = "file{0}".format(i)

    response = messages_pb2.ServerMessage()
    response.fuse_response.file_children.CopyFrom(repl)
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    with reply(endpoint, [getattr_response, response]):
        assert 0 == fl.readdir('/random/path', children)

    assert len(children) == children_num


def test_write_should_save_blocks(endpoint, fl):
    do_open(endpoint, fl, size=0)
    assert 5 == fl.write('/random/path', 0, 5)
    assert 5 == fl.read('/random/path', 0, 10)


def test_read_should_read_partial_content(endpoint, fl):
    do_open(endpoint, fl, blocks=[(4, 6)], size=10)

    assert 4 == fl.read('/random/path', 6, 4)


def test_read_should_request_synchronization(appmock_client, endpoint, fl):
    do_open(endpoint, fl, blocks=[(4, 6)], size=10)
    stream_id = get_stream_id_from_location_subscription(endpoint.history()[0])
    location_update_event = prepare_location_update_event([(0, 10)], stream_id, 0)
    sync_req = prepare_synchronize_block(2, 5).SerializeToString()

    appmock_client.reset_tcp_history()
    with reply(endpoint, location_update_event, reply_to_async=True) as queue:
        fl.read('/random/path', 2, 5)
        client_message = queue.get()

    assert client_message.SerializeToString() == sync_req


def test_read_should_continue_reading_after_synchronization(appmock_client, endpoint, fl):
    do_open(endpoint, fl, blocks=[(4, 6)], size=10)
    stream_id = get_stream_id_from_location_subscription(endpoint.history()[0])
    location_update_event = prepare_location_update_event([(0, 10)], stream_id, 0)

    appmock_client.reset_tcp_history()
    with reply(endpoint, location_update_event, reply_to_async=True):
        assert 5 == fl.read('/random/path', 2, 5)


def test_read_should_should_open_file_block_once(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)

    fl.expect_call_sh_open("file1", 1)
    fl.expect_call_sh_open("file2", 1)

    assert 5 == fl.read('/random/path', 0, 5)
    assert 5 == fl.read('/random/path', 5, 5)

    assert 5 == fl.read('/random/path', 0, 5)
    assert 5 == fl.read('/random/path', 0, 5)

    assert 5 == fl.read('/random/path', 5, 5)
    assert 5 == fl.read('/random/path', 5, 5)

    assert fl.verify_and_clear_expectations()


def test_write_should_should_open_file_block_once(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)

    fl.expect_call_sh_open("file1", 1)
    fl.expect_call_sh_open("file2", 1)

    assert 5 == fl.write('/random/path', 0, 5)
    assert 5 == fl.write('/random/path', 5, 5)

    assert 5 == fl.write('/random/path', 0, 5)
    assert 5 == fl.write('/random/path', 0, 5)

    assert 5 == fl.write('/random/path', 5, 5)
    assert 5 == fl.write('/random/path', 5, 5)

    assert fl.verify_and_clear_expectations()


def test_release_should_release_open_file_blocks(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)
    assert 5 == fl.read('/random/path', 0, 5)
    assert 5 == fl.read('/random/path', 5, 5)

    fl.expect_call_sh_release("file1", 1)
    fl.expect_call_sh_release("file2", 1)

    assert 0 == fl.release('/random/path')

    assert fl.verify_and_clear_expectations()


def test_release_should_pass_helper_errors(endpoint, fl):
    do_open(endpoint, fl, blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)
    assert 5 == fl.read('/random/path', 0, 5)
    assert 5 == fl.read('/random/path', 5, 5)

    fl.expect_call_sh_release("file1", 1)
    fl.expect_call_sh_release("file2", 1)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.release('/random/path')

    assert 'Owner died' in str(excinfo.value)
    assert fl.verify_and_clear_expectations()


def test_release_should_send_release_message_if_handle_id_is_set(endpoint, fl):
    do_open(endpoint, fl, size=0, handle_id='handle_id')
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [release_response]) as queue:
        assert 0 == fl.release('/random/path')
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.HasField('release')


def test_release_should_not_send_release_message_if_handle_id_is_not_set(endpoint, fl):
    do_open(endpoint, fl, size=0)
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, []) as queue:
        assert 0 == fl.release('/random/path')
        assert queue.empty()


def test_release_should_clear_handle_id_if_set(endpoint, fl):
    do_open(endpoint, fl, size=0, handle_id='handle_id')
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [release_response]):
        assert 0 == fl.release('/random/path')

    assert fl.open('/random/path', 0) >= 0
    with reply(endpoint, []) as queue:
        assert 0 == fl.release('/random/path')
        assert queue.empty()
