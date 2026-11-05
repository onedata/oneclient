"""This module tests events emission and subscription using event manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import time

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))

from test_common import *
import events
from proto import messages_pb2, common_messages_pb2, fuse_messages_pb2, \
                  event_messages_pb2

class EvtParam(Parameter):
    @staticmethod
    def evt_num(num):
        return Parameter('evt_num', 'Number of emitted events.', num)

    @staticmethod
    def key_num(num):
        return Parameter('key_num', 'Number of different event keys.', num)

    @staticmethod
    def emit_time(duration, unit='ms'):
        value = getattr(Duration, unit)(duration)
        return Parameter('emit_time',
                         'Summary time between the first and the last emitted '
                         'event.', value, unit)

    @staticmethod
    def evtps(evt_num, duration):
        return Parameter('evtps', 'Events emission speed.',
                         evt_num * 1. / duration.s(), 'evt/s')


@pytest.fixture
def uuid():
    return random_str()


@pytest.fixture
def sid():
    return random_int()


@pytest.fixture
def offset():
    return random_int()


@pytest.fixture
def size():
    return random_int()


@pytest.fixture
def time_thr():
    return random_int()


@pytest.fixture
def endpoint(appmock_client):
    return appmock_client.tcp_endpoint(443)


@pytest.fixture
def manager(endpoint):
    mgr = events.Manager(endpoint.ip, endpoint.port)
    yield mgr
    mgr.stop()


def prepare_status_response():
    server_response = messages_pb2.ServerMessage()
    server_response.fuse_response.status.code = common_messages_pb2.Status.ok

    return server_response


def prepare_file_read_subscription(sid, counter_thr=None, time_thr=None):
    msg = messages_pb2.ServerMessage()
    msg.subscription.id = sid
    if counter_thr:
        msg.subscription.file_read.counter_threshold = counter_thr
    if time_thr:
        msg.subscription.file_read.time_threshold = time_thr

    return msg


def prepare_file_written_subscription(sid, counter_thr=None, time_thr=None):
    msg = messages_pb2.ServerMessage()
    msg.subscription.id = sid
    if counter_thr:
        msg.subscription.file_written.counter_threshold = counter_thr
    if time_thr:
        msg.subscription.file_written.time_threshold = time_thr

    return msg


def prepare_cancellation(sid):
    msg = messages_pb2.ServerMessage()
    msg.subscription_cancellation.id = sid

    return msg


def prepare_file_attr_changed_event(uuid):
    evt = event_messages_pb2.Event()
    evt.file_attr_changed.file_attr.uuid = uuid.encode('utf-8')
    evt.file_attr_changed.file_attr.name = b'filename'
    evt.file_attr_changed.file_attr.mode = random_int(upper_bound=1023)
    evt.file_attr_changed.file_attr.uid = random_int(upper_bound=20000)
    evt.file_attr_changed.file_attr.gid = random_int(upper_bound=20000)
    evt.file_attr_changed.file_attr.mtime = int(time.time()) - random_int(upper_bound=1000000)
    evt.file_attr_changed.file_attr.atime = evt.file_attr_changed.file_attr.mtime - random_int(upper_bound=1000000)
    evt.file_attr_changed.file_attr.ctime = evt.file_attr_changed.file_attr.atime - random_int(upper_bound=1000000)
    evt.file_attr_changed.file_attr.type = fuse_messages_pb2.REG
    evt.file_attr_changed.file_attr.size = random_int(upper_bound=1000000000)
    evt.file_attr_changed.file_attr.owner_id = b''
    evt.file_attr_changed.file_attr.provider_id = b''
    evt.file_attr_changed.file_attr.index = b''

    return prepare_events([evt])


def prepare_file_location_changed_event(uuid):
    evt = event_messages_pb2.Event()
    evt.file_location_changed.file_location.uuid = uuid.encode('utf-8')
    evt.file_location_changed.file_location.space_id = 'space1'.encode('utf-8')
    evt.file_location_changed.file_location.storage_id = 'storage1'.encode('utf-8')
    evt.file_location_changed.file_location.file_id = 'file1'.encode('utf-8')
    evt.file_location_changed.file_location.provider_id = 'provider1'.encode('utf-8')
    evt.file_location_changed.file_location.version = 1

    return prepare_events([evt])


def prepare_quota_exceeded_event():
    quota_evt = event_messages_pb2.QuotaExceededEvent()

    evt = event_messages_pb2.Event()
    evt.quota_exceeded.CopyFrom(quota_evt)

    return prepare_events([evt])


def prepare_file_perm_changed_event(uuid):
    perm_evt = event_messages_pb2.FilePermChangedEvent()
    perm_evt.file_uuid = uuid.encode('utf-8')

    evt = event_messages_pb2.Event()
    evt.file_perm_changed.CopyFrom(perm_evt)

    return prepare_events([evt])


def prepare_file_renamed_event(uuid):
    top_entry = common_messages_pb2.FileRenamedEntry()
    top_entry.old_uuid = uuid.encode('utf-8')
    top_entry.new_uuid = random_str().encode('utf-8')
    top_entry.new_parent_uuid = random_str().encode('utf-8')
    top_entry.new_name = random_str().encode('utf-8')

    rename_evt = event_messages_pb2.FileRenamedEvent()
    rename_evt.top_entry.CopyFrom(top_entry)

    evt = event_messages_pb2.Event()
    evt.file_renamed.CopyFrom(rename_evt)

    return prepare_events([evt])


def prepare_file_removed_event(uuid):
    remove_evt = event_messages_pb2.FileRemovedEvent()
    remove_evt.file_uuid = uuid.encode('utf-8')

    evt = event_messages_pb2.Event()
    evt.file_removed.CopyFrom(remove_evt)

    return prepare_events([evt])


def prepare_events(evt_list):
    evts = event_messages_pb2.Events()
    evts.events.extend(evt_list)

    msg = messages_pb2.ServerMessage()
    msg.events.CopyFrom(evts)

    return msg

# -----------------------------------------------------------------------------

def test_subscribe_file_read(endpoint, manager, sid):
    for retry in range(10, 0, -1):
        sub = prepare_file_read_subscription(sid, 1, 1)
        with send(endpoint, sub):
            try:
                wait_until(lambda: manager.existsSubscription(sid, True))
                break
            except Exception:
                if retry == 1:
                    raise
                else:
                    pass


def test_cancel_file_read_subscription(endpoint, manager):
    sid = manager.subscribeFileRead(1, -1)
    wait_until(lambda: manager.existsSubscription(sid, True))

    can = prepare_cancellation(sid)
    with send(endpoint, can):
        wait_until(lambda: manager.existsSubscription(sid, False))


def test_counter_emit_file_read(endpoint, manager, uuid, offset, size):
    manager.subscribeFileRead(1, -1)
    _test_emit_file_read(endpoint, manager, uuid, offset, size)


def test_timed_emit_file_read(endpoint, manager, uuid, offset, size):
    manager.subscribeFileRead(-1, 500)
    _test_emit_file_read(endpoint, manager, uuid, offset, size)


def test_subscribe_file_written(endpoint, manager, sid):
    sub = prepare_file_written_subscription(sid, 1, 1)
    with send(endpoint, sub):
        wait_until(lambda: manager.existsSubscription(sid, True))


def test_cancel_file_written_subscription(endpoint, manager):
    sid = manager.subscribeFileWritten(1, -1)
    wait_until(lambda: manager.existsSubscription(sid, True))

    can = prepare_cancellation(sid)
    with send(endpoint, can):
        wait_until(lambda: manager.existsSubscription(sid, False))


def test_counter_emit_file_written(endpoint, manager, uuid, offset, size):
    manager.subscribeFileWritten(1, -1)
    _test_emit_file_written(endpoint, manager, uuid, offset, size)


def test_timed_emit_file_written(endpoint, manager, uuid, offset, size):
    manager.subscribeFileWritten(-1, 500)
    _test_emit_file_written(endpoint, manager, uuid, offset, size)


def test_counter_emit_file_truncated(endpoint, manager, uuid, size):
    manager.subscribeFileWritten(1, -1)
    _test_emit_file_truncated(endpoint, manager, uuid, size)


def test_timed_emit_file_truncated(endpoint, manager, uuid, size):
    manager.subscribeFileWritten(-1, 500)
    _test_emit_file_truncated(endpoint, manager, uuid, size)


def test_subscribe_file_attr_changed(endpoint, manager, uuid, time_thr):
    ok = prepare_status_response()
    with reply(endpoint, ok) as queue:
        manager.subscribeFileAttrChanged(uuid, time_thr)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_attr_changed')

    sub = client_message.subscription.file_attr_changed
    assert sub.file_uuid.decode('utf-8') == uuid
    assert sub.time_threshold == time_thr


def test_timed_emit_file_attr_changed(endpoint, manager, uuid):
    msg = prepare_status_response()
    evt = prepare_file_attr_changed_event(uuid)
    with reply(endpoint, msg):
        manager.subscribeFileAttrChanged(uuid, 500)

    with send(endpoint, evt):
        pass

    wait_until(lambda: manager.getHandlerCallsNumber("file_attr_changed") == 1)


def test_cancel_file_attr_changed_subscription(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, [msg]):
        sid = manager.subscribeFileAttrChanged(uuid, 500)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_location_changed(endpoint, manager, uuid, time_thr):
    msg = prepare_status_response()
    with reply(endpoint, msg) as queue:
        manager.subscribeFileLocationChanged(uuid, time_thr)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_location_changed')

    sub = client_message.subscription.file_location_changed
    assert sub.file_uuid.decode('utf-8') == uuid
    assert sub.time_threshold == time_thr


# @pytest.mark.skip
def test_timed_emit_file_location_changed(endpoint, manager, uuid):
    msg = prepare_status_response()
    evt = prepare_file_location_changed_event(uuid)
    with reply(endpoint, [msg, evt], reply_to_async=True):
        manager.subscribeFileLocationChanged(uuid, 500)

    wait_until(lambda: manager.getHandlerCallsNumber("file_location_changed") == 1)


def test_cancel_file_location_changed_subscription(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, msg):
        sid = manager.subscribeFileLocationChanged(uuid, 500)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_perm_changed(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, msg) as queue:
        manager.subscribeFilePermChanged(uuid)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_perm_changed')

    sub = client_message.subscription.file_perm_changed
    assert sub.file_uuid.decode('utf-8') == uuid


# @pytest.mark.skip
def test_timed_emit_file_perm_changed(endpoint, manager, uuid):
    msg = prepare_status_response()
    evt = prepare_file_perm_changed_event(uuid)
    with reply(endpoint, [msg, evt], reply_to_async=True):
        manager.subscribeFilePermChanged(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_perm_changed") == 1)


def test_cancel_file_perm_changed_subscription(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, [msg]):
        sid = manager.subscribeFilePermChanged(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_renamed(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, [msg]) as queue:
        manager.subscribeFileRenamed(uuid)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_renamed')

    sub = client_message.subscription.file_renamed
    assert sub.file_uuid.decode('utf-8') == uuid


# @pytest.mark.skip
def test_timed_emit_file_renamed(endpoint, manager, uuid):
    msg = prepare_status_response()
    evt = prepare_file_renamed_event(uuid)
    with reply(endpoint, [msg, evt], reply_to_async=True):
        manager.subscribeFileRenamed(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_renamed") == 1)


def test_cancel_file_renamed_subscription(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, msg):
        sid = manager.subscribeFileRenamed(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_removed(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, msg) as queue:
        manager.subscribeFileRemoved(uuid)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_removed')

    sub = client_message.subscription.file_removed
    assert sub.file_uuid.decode('utf-8') == uuid


# @pytest.mark.skip
def test_timed_emit_file_removed(endpoint, manager, uuid):
    msg = prepare_status_response()
    evt = prepare_file_removed_event(uuid)
    with reply(endpoint, [msg, evt], reply_to_async=True):
        manager.subscribeFileRemoved(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_removed") == 1)


def test_cancel_file_removed_subscription(endpoint, manager, uuid):
    msg = prepare_status_response()
    with reply(endpoint, msg):
        sid = manager.subscribeFileRemoved(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_quota_exceeded(endpoint, manager):
    msg = prepare_status_response()
    with reply(endpoint, msg) as queue:
        manager.subscribeQuotaExceeded()
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('quota_exceeded')


# @pytest.mark.skip
def test_timed_emit_quota_exceeded(endpoint, manager):
    msg = prepare_status_response()
    evt = prepare_quota_exceeded_event()
    with reply(endpoint, [msg, evt], reply_to_async=True):
        manager.subscribeQuotaExceeded()

    wait_until(lambda: manager.getHandlerCallsNumber("quota_exceeded") == 1)


def test_cancel_quota_exceeded_subscription(endpoint, manager):
    msg = prepare_status_response()
    with reply(endpoint, msg):
        sid = manager.subscribeQuotaExceeded()

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_aggregate_events(result, endpoint, manager):
    evt_num = 10000
    key_num = 100
    manager.subscribeFileRead(evt_num, -1)
    uuids = [random_str() for _ in range(key_num)]
    evts_per_uuid = int(evt_num / key_num)
    evt_size = 10

    emit_time = Duration()
    with receive(endpoint, msgs_num=2) as queue:
        queue.get()
        with measure(emit_time):
            for offset in range(evts_per_uuid):
                for uuid in uuids:
                    manager.emitFileRead(uuid, evt_size * offset, evt_size)
        client_message = queue.get()

    assert client_message.HasField('events')
    assert len(client_message.events.events) == key_num

    for evt in client_message.events.events:
        assert evt.HasField('file_read')
        assert evt.file_read.counter == evts_per_uuid
        assert evt.file_read.size == evts_per_uuid * evt_size
        assert evt.file_read.file_uuid.decode('utf-8') in uuids
        uuids.remove(evt.file_read.file_uuid.decode('utf-8'))

    result.set([
        EvtParam.emit_time(emit_time),
        EvtParam.evtps(evt_num, emit_time),
    ])

# -----------------------------------------------------------------------------


def _test_emit_file_read(endpoint, manager, uuid, offset, size):
    with receive(endpoint, msgs_num=2) as queue:
        manager.emitFileRead(uuid, offset, size)
        queue.get() # Skip message_stream_reset message
        client_message = queue.get()

    assert client_message.HasField('events')
    assert client_message.events.events[0].HasField('file_read')

    evt = client_message.events.events[0].file_read
    assert evt.counter == 1
    assert evt.file_uuid.decode('utf-8') == uuid
    assert evt.size == size


def _test_emit_file_written(endpoint, manager, uuid, offset, size):
    with receive(endpoint, msgs_num=2) as queue:
        manager.emitFileWritten(uuid, offset, size)
        queue.get() # Skip message_stream_reset message
        client_message = queue.get()

    assert client_message.HasField('events')
    assert client_message.events.events[0].HasField('file_written')

    evt = client_message.events.events[0].file_written
    assert evt.counter == 1
    assert evt.file_uuid.decode('utf-8') == uuid
    assert evt.size == size


def _test_emit_file_truncated(endpoint, manager, uuid, file_size):
    with receive(endpoint, msgs_num=2) as queue:
        manager.emitFileTruncated(uuid, file_size)
        queue.get() # Skip message_stream_reset message
        client_message = queue.get()

    assert client_message.HasField('events')
    assert client_message.events.events[0].HasField('file_written')

    evt = client_message.events.events[0].file_written
    assert evt.counter == 1
    assert evt.file_uuid.decode('utf-8') == uuid
    assert evt.file_size == file_size
