from __future__ import print_function

import random

__author__ = "Konrad Zemek"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import hashlib
import os
import sys
from threading import Thread
from multiprocessing import Pool
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
    return appmock_client.tcp_endpoint(443)


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
    response = prepare_attr_response(uuid, fuse_messages_pb2.REG)
    with reply(endpoint, response):
        return fl.getattr(uuid)


@pytest.fixture
def parentStat(endpoint, fl, parentUuid):
    response = prepare_attr_response(parentUuid, fuse_messages_pb2.REG)
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


def prepare_sync_request(offset, size):
    block = common_messages_pb2.FileBlock()
    block.offset = offset
    block.size = size

    req = fuse_messages_pb2.SynchronizeBlockAndComputeChecksum()
    req.uuid = 'uuid1'
    req.block.CopyFrom(block)

    client_request = messages_pb2.ClientMessage()
    client_request.fuse_request.synchronize_block_and_compute_checksum.CopyFrom(req)

    return client_request


def prepare_attr_response(uuid, filetype, size=None):
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


def prepare_helper_response():
    repl = fuse_messages_pb2.HelperParams()
    repl.helper_name = 'null'

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.helper_params.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_location(uuid, blocks=[]):
    file_blocks = prepare_file_blocks(blocks)

    repl = fuse_messages_pb2.FileLocation()
    repl.uuid = uuid
    repl.space_id = 'space1'
    repl.storage_id = 'storage1'
    repl.file_id = 'file1'
    repl.provider_id = 'provider1'
    repl.blocks.extend(file_blocks)

    return repl


def prepare_location_response(uuid, blocks=[]):
    location = prepare_location(uuid, blocks)

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_location.CopyFrom(location)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_rename_response(new_uuid):
    repl = fuse_messages_pb2.FileRenamed()
    repl.new_uuid = new_uuid

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_renamed.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_processing_status_response(status):
    repl = messages_pb2.ProcessingStatus()
    repl.code = status

    server_response = messages_pb2.ServerMessage()
    server_response.processing_status.CopyFrom(repl)

    return server_response


def prepare_open_response(handle_id='handle_id'):
    repl = fuse_messages_pb2.FileOpened()
    repl.handle_id = handle_id

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.file_opened.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_file_children_attr_response(uuid, prefix, count):
    child_attrs = []
    for i in range(count):
        f = prepare_attr_response(uuid, fuse_messages_pb2.REG).\
                    fuse_response.file_attr
        f.uuid = random_str()
        f.name = prefix+str(i)
        child_attrs.append(f)

    response = fuse_messages_pb2.FileChildrenAttrs()
    response.child_attrs.extend(child_attrs)

    return response


def do_open(endpoint, fl, uuid, size=None, blocks=[], handle_id='handle_id'):
    attr_response = prepare_attr_response(uuid, fuse_messages_pb2.REG,
                                          size=size)
    location_response = prepare_location_response(uuid, blocks)
    open_response = prepare_open_response(handle_id)

    with reply(endpoint, [attr_response, location_response, open_response]):
        handle = fl.open(uuid, 0)
        assert handle >= 0
        return handle


def do_release(endpoint, fl, uuid, fh):
    fsync_response = messages_pb2.ServerMessage()
    fsync_response.fuse_response.status.code = common_messages_pb2.Status.ok

    release_response = messages_pb2.ServerMessage()
    release_response.fuse_response.status.code = common_messages_pb2.Status.ok

    result = None
    with reply(endpoint, [fsync_response, release_response]) as queue:
        fl.release(uuid, fh)
        result = queue
    return result


def get_stream_id_from_location_subscription(subscription_message_data):
    location_subsc = messages_pb2.ClientMessage()
    location_subsc.ParseFromString(subscription_message_data)
    return location_subsc.message_stream.stream_id


def test_getattrs_should_get_attrs(endpoint, fl, uuid):
    response = prepare_attr_response(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, response) as queue:
        stat = fl.getattr(uuid)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')

    fuse_request = client_message.fuse_request
    assert fuse_request.file_request.HasField('get_file_attr')
    assert fuse_request.file_request.context_guid == uuid

    repl = response.fuse_response.file_attr
    assert repl.uuid == uuid
    assert stat.atime == repl.atime
    assert stat.mtime == repl.mtime
    assert stat.ctime == repl.ctime
    assert stat.gid == repl.gid
    assert stat.uid == repl.uid
    assert stat.mode == repl.mode | fslogic.regularMode()
    assert stat.size == repl.size


def test_getattrs_should_pass_errors(endpoint, fl, uuid):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr(uuid)

    assert 'No such file or directory' in str(excinfo.value)


def test_getattrs_should_cache_attrs(endpoint, fl, uuid):
    fuse_response = prepare_attr_response(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, fuse_response):
        stat = fl.getattr(uuid)

    new_stat = fl.getattr(uuid)

    assert stat == new_stat
    assert 3 == endpoint.all_messages_count()


def test_mkdir_should_mkdir(endpoint, fl):
    getattr_response = prepare_attr_response('parentUuid', fuse_messages_pb2.DIR)
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
    getattr_response = prepare_attr_response('parentUuid', fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.mkdir('parentUuid', 'name', 0123)

    assert 'Operation not permitted' in str(excinfo.value)


def test_rmdir_should_rmdir(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
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


def test_rmdir_should_pass_rmdir_errors(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rmdir('parentUuid', 'name')

    assert 'Operation not permitted' in str(excinfo.value)


def test_rename_should_rename(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    rename_response = prepare_rename_response('newUuid')

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


def test_rename_should_change_caches(appmock_client, endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    rename_response = prepare_rename_response('newUuid')

    with reply(endpoint, [getattr_response, rename_response]):
        fl.rename('parentUuid', 'name', 'newParentUuid', 'newName')

    stat = fl.getattr('newUuid')

    assert stat.size == getattr_response.fuse_response.file_attr.size
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getattr(uuid)

    assert 'No such file or directory' in str(excinfo.value)


def test_rename_should_pass_rename_errors(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.rename('parentUuid', 'name', 'newParentUuid', 'newName')

    assert 'Operation not permitted' in str(excinfo.value)


def test_chmod_should_change_mode(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, [response, response, getattr_response]) as queue:
        fl.chmod(uuid, 0123)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('change_mode')

    change_mode = file_request.change_mode
    assert change_mode.mode == 0123
    assert file_request.context_guid == \
           getattr_response.fuse_response.file_attr.uuid


def test_chmod_should_change_cached_mode(appmock_client, endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, getattr_response):
        stat = fl.getattr(uuid)

    assert stat.mode == getattr_response.fuse_response.file_attr.mode | \
                        fslogic.regularMode()
    assert 3 == endpoint.all_messages_count()
    appmock_client.reset_tcp_history()

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    with reply(endpoint, [response, response]):
        fl.chmod(uuid, 0356)

    stat = fl.getattr(uuid)

    assert stat.mode == 0356 | fslogic.regularMode()


def test_chmod_should_pass_chmod_errors(endpoint, fl, uuid):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enoent

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.chmod(uuid, 0312)

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
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.REG)

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
    #
    # Prepare first response with 5 files
    #
    repl1 = prepare_file_children_attr_response(uuid, "afiles-", 5)
    repl1.is_last = False

    response1 = messages_pb2.ServerMessage()
    response1.fuse_response.file_children_attrs.CopyFrom(repl1)
    response1.fuse_response.status.code = common_messages_pb2.Status.ok

    #
    # Prepare second response with another 5 file
    #
    repl2 = prepare_file_children_attr_response(uuid, "bfiles-", 5)
    repl2.is_last = True

    response2 = messages_pb2.ServerMessage()
    response2.fuse_response.file_children_attrs.CopyFrom(repl2)
    response2.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    offset = 0
    chunk_size = 50
    with reply(endpoint, [response1, response2]) as queue:
        children_chunk = fl.readdir(uuid, chunk_size, offset)
        _ = queue.get()
        assert len(children_chunk) == 12

    #
    # Immediately after the last request the value should be available
    # from readdir cache, without any communication with provider
    #
    for i in range(3):
        with reply(endpoint, []) as queue:
            children_chunk = fl.readdir(uuid, 5, 0)
            assert len(children_chunk) == 5
        time.sleep(1)

    #
    # After time validity has passed, the cache should be empty again
    #
    time.sleep(2)

    repl4 = fuse_messages_pb2.FileChildrenAttrs()
    repl4.child_attrs.extend([])

    response4 = messages_pb2.ServerMessage()
    response4.fuse_response.file_children_attrs.CopyFrom(repl4)
    response4.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    with reply(endpoint, [response4]) as queue:
        children_chunk = fl.readdir(uuid, 5, 0)
        assert len(children_chunk) == 2
        children += children_chunk

    assert sorted(children) == sorted(['..', '.'])


def test_readdir_should_return_unique_entries(endpoint, fl, uuid, stat):
    #
    # Prepare first response with 5 files
    #
    repl1 = prepare_file_children_attr_response(uuid, "afiles-", 5)
    repl1.is_last = False

    response1 = messages_pb2.ServerMessage()
    response1.fuse_response.file_children_attrs.CopyFrom(repl1)
    response1.fuse_response.status.code = common_messages_pb2.Status.ok

    #
    # Prepare second response with the same 5 files
    #
    repl2 = prepare_file_children_attr_response(uuid, "afiles-", 5)
    repl2.is_last = True

    response2 = messages_pb2.ServerMessage()
    response2.fuse_response.file_children_attrs.CopyFrom(repl2)
    response2.fuse_response.status.code = common_messages_pb2.Status.ok

    children = []
    offset = 0
    chunk_size = 50
    with reply(endpoint, [response1, response2]) as queue:
        children_chunk = fl.readdir(uuid, chunk_size, offset)
        _ = queue.get()
        children.extend(children_chunk)

    assert len(children) == 5 + 2


def test_readdir_should_pass_readdir_errors(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.readdir(uuid, 1024, 0)

    assert 'Operation not permitted' in str(excinfo.value)


def test_mknod_should_make_new_location(endpoint, fl, uuid, parentUuid, parentStat):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.REG)

    with reply(endpoint, [getattr_response]) as queue:
        fl.mknod(parentUuid, 'childName', 0762)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('make_file')

    make_file = file_request.make_file
    assert make_file.name == 'childName'
    assert make_file.mode == 0762
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

    assert 5 == len(fl.read(uuid, fh, 0, 5))


def test_read_should_read_zero_on_eof(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(0, 10)])

    assert 10 == len(fl.read(uuid, fh, 0, 12))
    assert 0 == len(fl.read(uuid, fh, 10, 2))


def test_read_should_pass_helper_errors(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(0, 10)])

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.read(uuid, fh, 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_write_should_write(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(0, 10)])

    assert 5 == fl.write(uuid, fh, 0, 5)


def test_write_should_change_file_size(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=5, blocks=[(0, 5)])
    assert 20 == fl.write(uuid, fh, 10, 20)

    stat = fl.getattr(uuid)
    assert 30 == stat.size


def test_write_should_pass_helper_errors(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(0, 10)])

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        fl.write(uuid, fh, 0, 10)

    assert 'Owner died' in str(excinfo.value)


def test_truncate_should_truncate(endpoint, fl, uuid, stat):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok
    location_response = prepare_location_response(uuid)

    with reply(endpoint, [response, location_response]) as queue:
        fl.truncate(uuid, 4)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('truncate')

    truncate = file_request.truncate
    assert truncate.size == 4
    assert file_request.context_guid == uuid


def test_truncate_should_pass_truncate_errors(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.REG)
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.eperm

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, [getattr_response, response]):
            fl.truncate(uuid, 3)

    assert 'Operation not permitted' in str(excinfo.value)


@pytest.mark.skip(reason="TODO VFS-3718")
def test_readdir_big_directory(endpoint, fl, uuid, stat):
    chunk_size = 2500
    children_num = 10*chunk_size

    # Prepare an array of responses of appropriate sizes to client
    # requests
    responses = []
    for i in xrange(0, children_num/chunk_size):
        repl = fuse_messages_pb2.FileChildrenAttrs()
        for j in xrange(0, chunk_size):
            link = prepare_attr_response(uuid, fuse_messages_pb2.REG).\
                        fuse_response.file_attr
            link.uuid = "childUuid_"+str(i)+"_"+str(j)
            link.name = "file_"+str(i)+"+"+str(j)
            repl.child_attrs.extend([link])

        response = messages_pb2.ServerMessage()
        response.fuse_response.file_children_attrs.CopyFrom(repl)
        response.fuse_response.status.code = common_messages_pb2.Status.ok

        responses.append(response)

    empty_repl = fuse_messages_pb2.FileChildrenAttrs()
    empty_repl.child_attrs.extend([])
    empty_repl.is_last = True
    empty_response = messages_pb2.ServerMessage()
    empty_response.fuse_response.file_children_attrs.CopyFrom(empty_repl)
    empty_response.fuse_response.status.code = common_messages_pb2.Status.ok

    responses.append(empty_response)

    assert len(responses) == children_num/chunk_size + 1

    children = []
    offset = 0
    with reply(endpoint, responses) as queue:
        while True:
            children_chunk = fl.readdir(uuid, chunk_size, offset)
            client_message = queue.get()
            children.extend(children_chunk)
            if len(children_chunk) < chunk_size:
                break
            offset += len(children_chunk)

    assert len(children) == children_num + 2


def test_write_should_save_blocks(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=0)
    assert 5 == fl.write(uuid, fh, 0, 5)
    assert 5 == len(fl.read(uuid, fh, 0, 10))


def test_read_should_read_partial_content(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(4, 6)])
    data = fl.read(uuid, fh, 6, 4)

    assert len(data) == 4


def test_read_should_request_synchronization(appmock_client, endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(4, 6)])
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


def test_read_should_continue_reading_after_synchronization(appmock_client,
                                                            endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[(4, 6)])
    sync_response = prepare_sync_response(uuid, '', [(0, 10)])

    appmock_client.reset_tcp_history()
    with reply(endpoint, sync_response):
        assert 5 == len(fl.read(uuid, fh, 2, 5))


def test_read_should_should_open_file_block_once(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[
        (0, 5, 'storage1', 'file1'), (5, 5, 'storage2', 'file2')])

    fl.expect_call_sh_open("file1", 1)
    fl.expect_call_sh_open("file2", 1)

    assert 5 == len(fl.read(uuid, fh, 0, 5))
    assert 5 == len(fl.read(uuid, fh, 5, 5))

    assert 5 == len(fl.read(uuid, fh, 0, 5))
    assert 5 == len(fl.read(uuid, fh, 0, 5))

    assert 5 == len(fl.read(uuid, fh, 5, 5))
    assert 5 == len(fl.read(uuid, fh, 5, 5))

    assert fl.verify_and_clear_expectations()


def test_release_should_release_open_file_blocks(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[
        (0, 5, 'storage1', 'file1'), (5, 5, 'storage2', 'file2')])

    assert 5 == len(fl.read(uuid, fh, 0, 5))
    assert 5 == len(fl.read(uuid, fh, 5, 5))

    fl.expect_call_sh_release('file1', 1)
    fl.expect_call_sh_release('file2', 1)

    do_release(endpoint, fl, uuid, fh)

    assert fl.verify_and_clear_expectations()


def test_release_should_pass_helper_errors(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=10, blocks=[
        (0, 5, 'storage1', 'file1'), (5, 5, 'storage2', 'file2')])

    assert 5 == len(fl.read(uuid, fh, 0, 5))
    assert 5 == len(fl.read(uuid, fh, 5, 5))

    fl.expect_call_sh_release('file1', 1)
    fl.expect_call_sh_release('file2', 1)

    with pytest.raises(RuntimeError) as excinfo:
        fl.failHelper()
        do_release(endpoint, fl, uuid, fh)

    assert 'Owner died' in str(excinfo.value)
    assert fl.verify_and_clear_expectations()


def test_release_should_send_release_message(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=0)

    sent_messages = do_release(endpoint, fl, uuid, fh)

    sent_messages.get() # skip fsync message
    client_message = sent_messages.get()
    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    assert client_message.fuse_request.file_request.HasField('release')


def test_release_should_send_fsync_message(endpoint, fl, uuid):
    fh = do_open(endpoint, fl, uuid, size=0)

    sent_messages = do_release(endpoint, fl, uuid, fh)

    client_message = sent_messages.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    assert client_message.fuse_request.file_request.HasField('fsync')


def test_fslogic_should_handle_processing_status_message(endpoint, fl, uuid):
    getattr_response = prepare_attr_response(uuid, fuse_messages_pb2.DIR)
    rename_response = prepare_rename_response('newUuid')
    processing_status_responses = \
        [prepare_processing_status_response(messages_pb2.IN_PROGRESS)
                for _ in range(5)]

    responses = [getattr_response]
    responses.extend(processing_status_responses)
    responses.append(rename_response)
    with reply(endpoint, responses) as queue:
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


def prepare_listxattr_response(uuid):
    repl = fuse_messages_pb2.XattrList()

    repl.names.extend(["xattr1", "xattr2", "xattr3", "xattr4"])

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.xattr_list.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def test_listxattrs_should_return_listxattrs(endpoint, fl, uuid):
    response = prepare_listxattr_response(uuid)

    listxattrs = []
    with reply(endpoint, response) as queue:
        listxattrs = fl.listxattr(uuid)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('list_xattr')
    assert file_request.context_guid == uuid

    assert response.status.code == common_messages_pb2.Status.ok
    assert len(listxattrs) == 4
    assert listxattrs[0] == "xattr1"
    assert listxattrs[3] == "xattr4"


def prepare_getxattr_response(uuid, name, value):
    repl = fuse_messages_pb2.Xattr()

    repl.name = name
    repl.value = value

    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.xattr.CopyFrom(repl)
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def test_getxattr_should_return_xattr(endpoint, fl, uuid):
    xattr_name = "org.onedata.acl"
    xattr_value = "READ | WRITE | DELETE"
    response = prepare_getxattr_response(uuid, xattr_name, xattr_value)

    xattr = None
    with reply(endpoint, response) as queue:
        xattr = fl.getxattr(uuid, xattr_name)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('get_xattr')

    assert xattr.name == xattr_name
    assert xattr.value == xattr_value


def test_getxattr_should_return_enoattr_for_invalid_xattr(endpoint, fl, uuid):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enodata

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.getxattr(uuid, "org.onedata.dontexist")

    assert 'No data available' in str(excinfo.value)


def test_setxattr_should_set_xattr(endpoint, fl, uuid):
    xattr_name = "org.onedata.acl"
    xattr_value = "READ | WRITE | DELETE"
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, response) as queue:
        fl.setxattr(uuid, xattr_name, xattr_value, False, False)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')

    file_request = client_message.fuse_request.file_request
    assert file_request.HasField('set_xattr')
    assert file_request.set_xattr.HasField('xattr')

    assert file_request.set_xattr.xattr.name == xattr_name
    assert file_request.set_xattr.xattr.value == xattr_value


def test_setxattr_should_set_xattr_with_binary_data(endpoint, fl, uuid):
    xattr_name = "org.onedata.acl"
    xattr_value = b'BEGINSTRINGWITHNULLS\x00\x0F\x00\x0F\x00\x0F\x00\x0F\x00\x0F\x00\x0FENDSTRINGWITHNULLS'
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, response) as queue:
        fl.setxattr(uuid, xattr_name, xattr_value, False, False)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    file_request = client_message.fuse_request.file_request

    assert file_request.HasField('set_xattr')
    assert file_request.set_xattr.HasField('xattr')

    assert file_request.set_xattr.xattr.name == xattr_name
    assert file_request.set_xattr.xattr.value == xattr_value


def test_setxattr_should_set_xattr_with_long_value(endpoint, fl, uuid):
    xattr_name = "org.onedata.acl"
    xattr_value = "askljdhflajkshdfjklhasjkldfhajklshdfljkashdfjklhasljkdhfjklashdfjklhasljdfhljkashdfljkhasjkldfhkljasdfhaslkdhfljkashdfljkhasdjklfhajklsdhfljkashdflkjhasjkldfhlakjsdhflkjahsfjklhasdjklfghlajksdgjklashfjklashfljkahsdljkfhasjkldfhlkajshdflkjahsdfljkhasldjkfhlkashdflkjashdfljkhasldkjfhalksdhfljkashdfljkhasdlfjkhaljksdhfjklashdfjklhasjkldfhljkasdhfljkashdlfjkhasldjkfhaljskdhfljkashdfljkhaspeuwshfiuawhgelrfihjasdgffhjgsdfhjgaskhjdfgjkaszgdfjhasdkfgaksjdfgkjahsdgfkhjasgdfkjhagsdkhjfgakhsjdfgkjhasgdfkhjgasdkjhfgakjshdgfkjhasgdkjhfgaskjhdfgakjhsdgfkjhasdgfkjhagsdkfhjgaskjdhfgkajsgdfkhjagsdkfjhgasdkjhfgaksjhdgfkajshdgfkjhasdgfkjhagskjdhfgakjshdgfkhjasdgfkjhasgdkfhjgaskdhjfgaksjdfgkasjdhgfkajshdgfkjhasgdfkhjagskdhjfgaskhjdfgkjasdhgfkjasgdkhjasdgkfhjgaksjhdfgkajshdgfkjhasdgfkjhagsdhjkfgaskhjdfgahjksdgfkhjasdgfhasgdfjhgaskdhjfgadkshjgfakhjsdgfkjhadsgkfhjagshjkdfgadhjsaskljdhflajkshdfjklhasjkldfhajklshdfljkashdfjklhasljkdhfjklashdfjklhasljdfhljkashdfljkhasjkldfhkljasdfhaslkdhfljkashdfljkhasdjklfhajklsdhfljkashdflkjhasjkldfhlakjsdhflkjahsfjklhasdjklfghlajksdgjklashfjklashfljkahsdljkfhasjkldfhlkajshdflkjahsdfljkhasldjkfhlkashdflkjashdfljkhasldkjfhalksdhfljkashdfljkhasdlfjkhaljksdhfjklashdfjklhasjkldfhljkasdhfljkashdlfjkhasldjkfhaljskdhfljkashdfljkhaspeuwshfiuawhgelrfihjasdgffhjgsdfhjgaskhjdfgjkaszgdfjhasdkfgaksjdfgkjahsdgfkhjasgdfkjhagsdkhjfgakhsjdfgkjhasgdfkhjgasdkjhfgakjshdgfkjhasgdkjhfgaskjhdfgakjhsdgfkjhasdgfkjhagsdkfhjgaskjdhfgkajsgdfkhjagsdkfjhgasdkjhfgaksjhdgfkajshdgfkjhasdgfkjhagskjdhfgakjshdgfkhjasdgfkjhasgdkfhjgaskdhjfgaksjdfgkasjdhgfkajshdgfkjhasgdfkhjagskdhjfgaskhjdfgkjasdhgfkjasgdkhjasdgkfhjgaksjhdfgkajshdgfkjhasdgfkjhagsdhjkfgaskhjdfgahjksdgfkhjasdgfhasgdfjhgaskdhjfgadkshjgfakhjsdgfkjhadsgkfhjagshjkdfgadhjsaskljdhflajkshdfjklhasjkldfhajklshdfljkashdfjklhasljkdhfjklashdfjklhasljdfhljkashdfljkhasjkldfhkljasdfhaslkdhfljkashdfljkhasdjklfhajklsdhfljkashdflkjhasjkldfhlakjsdhflkjahsfjklhasdjklfghlajksdgjklashfjklashfljkahsdljkfhasjkldfhlkajshdflkjahsdfljkhasldjkfhlkashdflkjashdfljkhasldkjfhalksdhfljkashdfljkhasdlfjkhaljksdhfjklashdfjklhasjkldfhljkasdhfljkashdlfjkhasldjkfhaljskdhfljkashdfljkhaspeuwshfiuawhgelrfihjasdgffhjgsdfhjgaskhjdfgjkaszgdfjhasdkfgaksjdfgkjahsdgfkhjasgdfkjhagsdkhjfgakhsjdfgkjhasgdfkhjgasdkjhfgakjshdgfkjhasgdkjhfgaskjhdfgakjhsdgfkjhasdgfkjhagsdkfhjgaskjdhfgkajsgdfkhjagsdkfjhgasdkjhfgaksjhdgfkajshdgfkjhasdgfkjhagskjdhfgakjshdgfkhjasdgfkjhasgdkfhjgaskdhjfgaksjdfgkasjdhgfkajshdgfkjhasgdfkhjagskdhjfgaskhjdfgkjasdhgfkjasgdkhjasdgkfhjgaksjhdfgkajshdgfkjhasdgfkjhagsdhjkfgaskhjdfgahjksdgfkhjasdgfhasgdfjhgaskdhjfgadkshjgfakhjsdgfkjhadsgkfhjagshjkdfgadhjsaskljdhflajkshdfjklhasjkldfhajklshdfljkashdfjklhasljkdhfjklashdfjklhasljdfhljkashdfljkhasjkldfhkljasdfhaslkdhfljkashdfljkhasdjklfhajklsdhfljkashdflkjhasjkldfhlakjsdhflkjahsfjklhasdjklfghlajksdgjklashfjklashfljkahsdljkfhasjkldfhlkajshdflkjahsdfljkhasldjkfhlkashdflkjashdfljkhasldkjfhalksdhfljkashdfljkhasdlfjkhaljksdhfjklashdfjklhasjkldfhljkasdhfljkashdlfjkhasldjkfhaljskdhfljkashdfljkhaspeuwshfiuawhgelrfihjasdgffhjgsdfhjgaskhjdfgjkaszgdfjhasdkfgaksjdfgkjahsdgfkhjasgdfkjhagsdkhjfgakhsjdfgkjhasgdfkhjgasdkjhfgakjshdgfkjhasgdkjhfgaskjhdfgakjhsdgfkjhasdgfkjhagsdkfhjgaskjdhfgkajsgdfkhjagsdkfjhgasdkjhfgaksjhdgfkajshdgfkjhasdgfkjhagskjdhfgakjshdgfkhjasdgfkjhasgdkfhjgaskdhjfgaksjdfgkasjdhgfkajshdgfkjhasgdfkhjagskdhjfgaskhjdfgkjasdhgfkjasgdkhjasdgkfhjgaksjhdfgkajshdgfkjhasdgfkjhagsdhjkfgaskhjdfgahjksdgfkhjasdgfhasgdfjhgaskdhjfgadkshjgfakhjsdgfkjhadsgkfhjagshjkdfgadhjs"

    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, response) as queue:
        fl.setxattr(uuid, xattr_name, xattr_value, False, False)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    file_request = client_message.fuse_request.file_request

    assert file_request.HasField('set_xattr')
    assert file_request.set_xattr.HasField('xattr')

    assert file_request.set_xattr.xattr.name == xattr_name
    assert file_request.set_xattr.xattr.value == xattr_value


def test_removexattr_should_remove_xattr(endpoint, fl, uuid):
    xattr_name = "org.onedata.acl"
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.ok

    with reply(endpoint, response) as queue:
        fl.removexattr(uuid, xattr_name)
        client_message = queue.get()

    assert client_message.HasField('fuse_request')
    assert client_message.fuse_request.HasField('file_request')
    file_request = client_message.fuse_request.file_request
    assert file_request.context_guid == uuid

    remove_xattr_request = file_request.remove_xattr
    assert remove_xattr_request.HasField('name')
    assert remove_xattr_request.name == xattr_name


def test_removexattr_should_return_enoattr_for_invalid_xattr(endpoint, fl, uuid):
    response = messages_pb2.ServerMessage()
    response.fuse_response.status.code = common_messages_pb2.Status.enodata

    with pytest.raises(RuntimeError) as excinfo:
        with reply(endpoint, response):
            fl.removexattr(uuid, "org.onedata.dontexist")

    assert 'No data available' in str(excinfo.value)
