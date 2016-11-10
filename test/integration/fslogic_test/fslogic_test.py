from __future__ import print_function

import random

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import hashlib
import os
import sys
from threading import Thread
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


@pytest.fixture
def uuid():
    return random_str()


@pytest.fixture
def parentUuid():
    return random_str()


@pytest.fixture
def stat(endpoint, fl, uuid):
    response = prepare_getattr(uuid, fuse_messages_pb2.REG)
    with reply(endpoint, response):
        return fl.getattr(uuid)


@pytest.fixture
def parentStat(endpoint, fl, parentUuid):
    response = prepare_getattr(parentUuid, fuse_messages_pb2.REG)
    with reply(endpoint, response):
        return fl.getattr(parentUuid)


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


def prepare_sync_response(uuid, data, blocks):
    md = hashlib.new('MD4')
    md.update(data)

    repl = fuse_messages_pb2.SyncResponse()
    repl.checksum = md.digest()
    repl.file_location.CopyFrom(prepare_location(uuid, blocks))

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.sync_response.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_synchronize_block(offset, size):
    block = common_messages_pb2.FileBlock()
    block.offset = offset
    block.size = size

    req = fuse_messages_pb2.SynchronizeBlockAndComputeChecksum()
    req.uuid = 'uuid1'
    req.block.CopyFrom(block)

    client_request = messages_pb2.ClientMessage()
    client_request.fuse_request.synchronize_block_and_compute_checksum.CopyFrom(req)

    return client_request


def prepare_getattr(uuid, filetype, size=None):
    repl = fuse_messages_pb2.FileAttr()
    repl.uuid = uuid
    repl.name = 'filename'
    repl.mode = random.randint(0, 1023)
    repl.uid = random.randint(0, 20000)
    repl.gid = random.randint(0, 20000)
    repl.mtime = int(time.time()) - random.randint(0, 1000000)
    repl.atime = repl.mtime - random.randint(0, 1000000)
    repl.ctime = repl.atime - random.randint(0, 1000000)
    repl.type = filetype
    repl.size = size if size else random.randint(0, 1000000000)
    repl.owner_id = ''
    repl.provider_id = ''

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


def prepare_location(uuid, blocks=[], handle_id=None):
    file_blocks = prepare_file_blocks(blocks)

    repl = fuse_messages_pb2.FileLocation()
    repl.uuid = uuid
    repl.space_id = 'space1'
    repl.storage_id = 'storage1'
    repl.file_id = 'file1'
    repl.provider_id = 'provider1'
    repl.blocks.extend(file_blocks)
    if handle_id is not None:
        repl.handle_id = handle_id

    return repl


def prepare_location_response(uuid, blocks=[], handle_id=None):
    repl = prepare_location(uuid, blocks, handle_id)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_location.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_rename(new_uuid):
    repl = fuse_messages_pb2.FileRenamed()
    repl.new_uuid = new_uuid

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_renamed.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def do_open(endpoint, fl, uuid, blocks=[], size=None, handle_id=None):
    getattr_response = prepare_getattr(uuid, fuse_messages_pb2.REG,
                                       size=size)

    open_response = prepare_location_response(uuid, blocks, handle_id)

    with reply(endpoint, [getattr_response, open_response]):
        handle = fl.open(uuid, 0)
        assert handle >= 0
        return handle


def do_read(fl, uuid, offset, size):
    fl.read(uuid, offset, size)


def get_stream_id_from_location_subscription(subscription_message_data):
    location_subsc = messages_pb2.ClientMessage()
    location_subsc.ParseFromString(subscription_message_data)
    return location_subsc.message_stream.stream_id


def test_getattrs_should_get_attrs(endpoint, fl):
    response = prepare_getattr('uuid', fuse_messages_pb2.REG)

    with reply(endpoint, response) as queue:
        stat = fl.getattr('uuid')
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.file_request.HasField('get_file_attr')
    assert fuse_request.file_request.context_guid == 'uuid'

    repl = response.fuse_response.file_attr
    assert repl.uuid == 'uuid'
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

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr('uuid')

    assert 'No such file or directory' in str(excinfo.value)


def test_getattrs_should_cache_attrs(endpoint, fl):
    fuse_response = prepare_getattr('uuid', fuse_messages_pb2.REG)

    with reply(endpoint, fuse_response):
        stat = fl.getattr('uuid')

    new_stat = fl.getattr('uuid')

    assert stat == new_stat
    assert 3 == endpoint.all_messages_count()


def test_mkdir_should_mkdir(endpoint, fl):
    getattr_response = prepare_getattr('parentUuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [response, getattr_response]) as queue:
        fl.mkdir('parentUuid', 'name', 0123)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.context_guid == 'parentUuid'

    assert file_request.HasField('create_dir')
    create_dir = file_request.create_dir
    assert create_dir.name == 'name'
    assert create_dir.mode == 0123
    assert file_request.context_guid == \
           getattr_response.fuse_response.file_attr.uuid


def test_mkdir_should_pass_mkdir_errors(endpoint, fl):
    getattr_response = prepare_getattr('parentUuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.mkdir('parentUuid', 'name', 0123)

    assert 'Operation not permitted' in str(excinfo.value)


def test_rmdir_should_rmdir(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [getattr_response, response]) as queue:
        fl.rmdir('parentUuid', 'name')
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('delete_file')
    assert file_request.context_guid == \
            getattr_response.fuse_response.file_attr.uuid


def test_rmdir_should_pass_rmdir_errors(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rmdir('parentUuid', 'name')

    assert 'Operation not permitted' in str(excinfo.value)


def test_rename_should_rename(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    rename_response = prepare_rename('newUuid')

    with reply(endpoint, [getattr_response, rename_response]) as queue:
        fl.rename('parentUuid', 'name', 'newParentUuid', 'newName')
        queue.get()
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('rename')

    rename = file_request.rename
    assert rename.target_parent_uuid == 'newParentUuid'
    assert rename.target_name == 'newName'
    assert file_request.context_guid == \
           getattr_response.fuse_response.file_attr.uuid


def test_rename_should_change_caches(appmock_client, endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    rename_response = prepare_rename('newUuid')

    with reply(endpoint, [getattr_response, rename_response]):
        fl.rename('parentUuid', 'name', 'newParentUuid', 'newName')

    stat = fl.getattr('newUuid')

    assert stat.size == getattr_response.fuse_response.file_attr.size
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr('uuid')

    assert 'No such file or directory' in str(excinfo.value)


def test_rename_should_pass_rename_errors(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rename('parentUuid', 'name', 'newParentUuid', 'newName')

    assert 'Operation not permitted' in str(excinfo.value)


def test_chmod_should_change_mode(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [response, response, getattr_response]) as queue:
        fl.chmod('uuid', 0123)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('change_mode')

    change_mode = file_request.change_mode
    assert change_mode.mode == 0123
    assert file_request.context_guid == \
           getattr_response.fuse_response.file_attr.uuid


def test_chmod_should_change_cached_mode(appmock_client, endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.REG)

    with reply(endpoint, getattr_response):
        stat = fl.getattr('uuid')

    assert stat.mode == getattr_response.fuse_response.file_attr.mode | \
                        fslogic.regularMode()
    assert 3 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    with reply(endpoint, [response, response]):
        fl.chmod('uuid', 0356)

    stat = fl.getattr('uuid')

    assert stat.mode == 0356 | fslogic.regularMode()


def test_chmod_should_pass_chmod_errors(endpoint, fl):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.chmod('uuid', 0312)

    assert 'No such file or directory' in str(excinfo.value)


def test_utime_should_update_times(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, response) as queue:
        fl.utime(uuid)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('update_times')

    update_times = file_request.update_times
    assert update_times.atime == update_times.mtime
    assert update_times.atime == update_times.ctime
    assert update_times.atime <= time.time()
    assert file_request.context_guid == uuid


def test_utime_should_change_cached_times(appmock_client, endpoint, fl, uuid):
    getattr_response = prepare_getattr(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, getattr_response):
        stat = fl.getattr(uuid)

    assert stat.atime == getattr_response.fuse_response.file_attr.atime
    assert stat.mtime == getattr_response.fuse_response.file_attr.mtime
    assert 3 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    with reply(endpoint, response):
        fl.utime(uuid)

    stat = fslogic.Stat()
    fl.getattr(uuid)

    assert stat.atime != getattr_response.fuse_response.file_attr.atime
    assert stat.mtime != getattr_response.fuse_response.file_attr.mtime


def test_utime_should_update_times_with_buf(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    ubuf = fslogic.Ubuf()
    ubuf.actime = 54321
    ubuf.modtime = 12345

    with reply(endpoint, response) as queue:
        fl.utime_buf(uuid, ubuf)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('update_times')

    update_times = file_request.update_times
    assert update_times.atime == ubuf.actime
    assert update_times.mtime == ubuf.modtime
    assert file_request.context_guid == uuid


def test_utime_should_pass_utime_errors(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.utime(uuid)

    assert 'Operation not permitted' in str(excinfo.value)

    ubuf = fslogic.Ubuf()
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.utime_buf(uuid, ubuf)

    assert 'Operation not permitted' in str(excinfo.value)


def test_readdir_should_read_dir(endpoint, fl, uuid, stat):
    file1 = common_messages_pb2.ChildLink()
    file1.uuid = "childUuid1"
    file1.name = "file1"

    file2 = common_messages_pb2.ChildLink()
    file2.uuid = "childUuid2"
    file2.name = "file2"

    repl = fuse_messages_pb2.FileChildren()
    repl.child_links.extend([file1, file2])

    response = messages_pb2.ServerMessage()
    response.fuse_response.file_children.CopyFrom(repl)
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    with reply(endpoint, response) as queue:
        children = fl.readdir(uuid)
        client_message = queue.get()

    assert sorted(children) == sorted([file1.name, file2.name, '..', '.'])

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('get_file_children')

    get_file_children = file_request.get_file_children
    assert get_file_children.offset == 0
    assert file_request.context_guid == uuid


def test_readdir_should_pass_readdir_errors(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.readdir(uuid)

    assert 'Operation not permitted' in str(excinfo.value)


def test_mknod_should_make_new_location(endpoint, fl, uuid, parentUuid, parentStat):
    mknod_response = prepare_location_response(uuid)
    getattr_response = prepare_getattr(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, [mknod_response, getattr_response]) as queue:
        fl.mknod(parentUuid, 'childName', 0762)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('get_new_file_location')

    get_new_file_location = file_request.get_new_file_location
    assert get_new_file_location.name == 'childName'
    assert get_new_file_location.mode == 0762
    assert file_request.context_guid == parentUuid


def test_mknod_should_pass_location_errors(endpoint, fl, parentUuid, parentStat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.mknod(parentUuid, 'childName', 0123)

    assert 'Operation not permitted' in str(excinfo.value)


def test_read_should_read(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(0, 10)])
    data = fl.read(uuid, fh, 0, 5)
    assert len(data) == 5


def test_read_should_read_zero_on_eof(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(0, 10)], size=10)
    assert 10 == len(fl.read(uuid, fh, 0, 12))
    assert 0 == len(fl.read(uuid, fh, 10, 2))


def test_read_should_pass_helper_errors(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(0, 10)], size=10)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.read(uuid, fh, 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_write_should_write(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(0, 10)], size=10)
    assert 5 == fl.write(uuid, fh, 0, 5)


def test_write_should_change_file_size(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', blocks=[(0, 5)], size=5)
    assert 20 == fl.write('uuid', fh, 10, 20)

    stat = fl.getattr('uuid')
    assert 30 == stat.size


def test_write_should_pass_helper_errors(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', blocks=[(0, 10)], size=10)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.write('uuid', fh, 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_truncate_should_truncate(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    loc_response = prepare_location_response(uuid)

    with reply(endpoint, [response, loc_response]) as queue:
        fl.truncate(uuid, 4)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('truncate')

    truncate = file_request.truncate
    assert truncate.size == 4
    assert file_request.context_guid == uuid


def test_truncate_should_pass_truncate_errors(endpoint, fl):
    getattr_response = prepare_getattr('uuid', fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.truncate('uuid', 3)

    assert 'Operation not permitted' in str(excinfo.value)


def test_readdir_big_directory(endpoint, fl, uuid, stat):
    children_num = 100000

    repl = fuse_messages_pb2.FileChildren()
    for i in xrange(0, children_num):
        link = repl.child_links.add()
        link.uuid = "uuid{0}".format(i)
        link.name = "file{0}".format(i)

    response = messages_pb2.ServerMessage()
    response.fuse_response.file_children.CopyFrom(repl)
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    empty_response = messages_pb2.ServerMessage()
    empty_response.fuse_response.file_children.CopyFrom(
        fuse_messages_pb2.FileChildren())
    empty_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [response, empty_response]):
        children = fl.readdir(uuid)

    assert len(children) == children_num + 2


def test_write_should_save_blocks(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=0)
    assert 5 == fl.write(uuid, fh, 0, 5)
    assert 5 == len(fl.read(uuid, fh, 0, 10))


def test_read_should_read_partial_content(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(4, 6)], size=10)

    assert 4 == len(fl.read(uuid, fh, 6, 4))


def test_read_should_request_synchronization(appmock_client, endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(4, 6)], size=10)
    sync_response = prepare_sync_response(uuid, '', [(0, 10)])

    appmock_client.reset_tcp_history()
    with reply(endpoint, sync_response) as queue:
        fl.read(uuid, fh, 2, 5)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('synchronize_block_and_compute_checksum')
    block = common_messages_pb2.FileBlock()
    block.offset = 2
    block.size = 5
    sync = file_request.synchronize_block_and_compute_checksum
    assert sync.block == block
    assert file_request.context_guid == uuid


def test_read_should_continue_reading_after_synchronization(appmock_client, endpoint,
                                                            fl, uuid):
    fh = do_open(endpoint, fl, uuid, blocks=[(4, 6)], size=10)
    sync_response = prepare_sync_response(uuid, '', [(0, 10)])

    appmock_client.reset_tcp_history()
    with reply(endpoint, sync_response):
        assert 5 == len(fl.read(uuid, fh, 2, 5))


def test_read_should_should_open_file_block_once(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)

    fl.expect_call_sh_open("file1", 1)
    fl.expect_call_sh_open("file2", 1)

    assert 5 == len(fl.read('uuid', fh, 0, 5))
    assert 5 == len(fl.read('uuid', fh, 5, 5))

    assert 5 == len(fl.read('uuid', fh, 0, 5))
    assert 5 == len(fl.read('uuid', fh, 0, 5))

    assert 5 == len(fl.read('uuid', fh, 5, 5))
    assert 5 == len(fl.read('uuid', fh, 5, 5))

    assert fl.verify_and_clear_expectations()


def test_release_should_release_open_file_blocks(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)
    assert 5 == len(fl.read('uuid', fh, 0, 5))
    assert 5 == len(fl.read('uuid', fh, 5, 5))

    fl.expect_call_sh_release("file1", 1)
    fl.expect_call_sh_release("file2", 1)

    fl.release('uuid', fh)

    assert fl.verify_and_clear_expectations()


def test_release_should_pass_helper_errors(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', blocks=[(0, 5, 'storage1', 'file1'),
                                       (5, 5, 'storage2', 'file2')], size=10)
    assert 5 == len(fl.read('uuid', fh, 0, 5))
    assert 5 == len(fl.read('uuid', fh, 5, 5))

    fl.expect_call_sh_release("file1", 1)
    fl.expect_call_sh_release("file2", 1)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.release('uuid', fh)

    assert 'Owner died' in str(excinfo.value)
    assert fl.verify_and_clear_expectations()


def test_release_should_send_release_message_if_handle_id_is_set(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', size=0, handle_id='handle_id')
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [release_response]) as queue:
        fl.release('uuid', fh)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    assert client_message.fuse_request.file_request.HasField('release')


def test_release_should_not_send_release_message_if_handle_id_is_not_set(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', size=0)
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, []) as queue:
        fl.release('uuid', fh)
        assert queue.empty()


def test_release_should_clear_handle_id_if_set(endpoint, fl):
    fh = do_open(endpoint, fl, 'uuid', size=0, handle_id='handle_id')
    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, release_response):
        fl.release('uuid', fh)

    fh = fl.open('uuid', 0)
    assert fh >= 0
    with reply(endpoint, []) as queue:
        fl.release('uuid', fh)
        assert queue.empty()


def test_location_with_different_flags(endpoint, fl, uuid, stat):
    O_RDONLY = 00
    O_WRONLY = 01
    O_RDWR = 02

    getattr_response = prepare_getattr(uuid, fuse_messages_pb2.REG, size=0)

    location_response = prepare_location_response(uuid, handle_id='id')

    with reply(endpoint, location_response):
        assert fl.open(uuid, O_RDONLY) >= 0

    # should not ask for location and will fail on try due to wrong response
    assert fl.open(uuid, O_RDONLY) >= 0

    # should try to get location for different flags and
    # fail due to wrong response
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, getattr_response):
            assert fl.open(uuid, O_WRONLY) >= 0
    assert 'file_location field missing: Protocol error' in str(excinfo.value)

    with reply(endpoint, location_response):
        assert fl.open(uuid, O_WRONLY) >= 0

    # should not ask for location and will fail on try due to wrong response
    with reply(endpoint, [getattr_response, getattr_response]):
        assert fl.open(uuid, O_WRONLY) >= 0

    # should try to get location for different flags and
    # fail due to wrong response
    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response]):
            assert fl.open(uuid, O_RDWR) >= 0
    assert 'file_location field missing: Protocol error' in str(excinfo.value)

    with reply(endpoint, location_response):
        assert fl.open(uuid, O_RDWR) >= 0

    # should not ask for location and will fail on try due to wrong response
    with reply(endpoint, [getattr_response, getattr_response]):
        assert fl.open(uuid, O_RDWR) >= 0
