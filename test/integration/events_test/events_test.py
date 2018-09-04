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
    return events.Manager(endpoint.ip, endpoint.port)


def prepare_file_read_subscription(sid, counter_thr=None, time_thr=None):
    read_sub = event_messages_pb2.FileReadSubscription()
    if counter_thr:
        read_sub.counter_threshold = counter_thr
    if time_thr:
        read_sub.time_threshold = time_thr

    sub = event_messages_pb2.Subscription()
    sub.file_read.CopyFrom(read_sub)

    return prepare_subscription(sid, sub)


def prepare_file_written_subscription(sid, counter_thr=None, time_thr=None):
    write_sub = event_messages_pb2.FileWrittenSubscription()
    if counter_thr:
        write_sub.counter_threshold = counter_thr
    if time_thr:
        write_sub.time_threshold = time_thr

    sub = event_messages_pb2.Subscription()
    sub.file_written.CopyFrom(write_sub)

    return prepare_subscription(sid, sub)


def prepare_subscription(sid, sub):
    sub.id = sid
    msg = messages_pb2.ServerMessage()
    msg.subscription.CopyFrom(sub)

    return msg


def prepare_cancellation(sid):
    can = event_messages_pb2.SubscriptionCancellation()
    can.id = sid

    msg = messages_pb2.ServerMessage()
    msg.subscription_cancellation.CopyFrom(can)

    return msg


def prepare_file_attr_changed_event(uuid):
    attr = fuse_messages_pb2.FileAttr()
    attr.uuid = uuid
    attr.name = 'filename'
    attr.mode = random_int(upper_bound=1023)
    attr.uid = random_int(upper_bound=20000)
    attr.gid = random_int(upper_bound=20000)
    attr.mtime = int(time.time()) - random_int(upper_bound=1000000)
    attr.atime = attr.mtime - random_int(upper_bound=1000000)
    attr.ctime = attr.atime - random_int(upper_bound=1000000)
    attr.type = fuse_messages_pb2.REG
    attr.size = random_int(upper_bound=1000000000)
    attr.owner_id = ''
    attr.provider_id = ''

    attr_evt = event_messages_pb2.FileAttrChangedEvent()
    attr_evt.file_attr.CopyFrom(attr)

    evt = event_messages_pb2.Event()
    evt.file_attr_changed.CopyFrom(attr_evt)

    return prepare_events([evt])


def prepare_file_location_changed_event(uuid):
    loc = fuse_messages_pb2.FileLocation()
    loc.uuid = uuid
    loc.space_id = 'space1'
    loc.storage_id = 'storage1'
    loc.file_id = 'file1'
    loc.provider_id = 'provider1'
    loc.version = 1

    loc_evt = event_messages_pb2.FileLocationChangedEvent()
    loc_evt.file_location.CopyFrom(loc)

    evt = event_messages_pb2.Event()
    evt.file_location_changed.CopyFrom(loc_evt)

    return prepare_events([evt])


def prepare_quota_exceeded_event():
    quota_evt = event_messages_pb2.QuotaExceededEvent()

    evt = event_messages_pb2.Event()
    evt.quota_exceeded.CopyFrom(quota_evt)

    return prepare_events([evt])


def prepare_file_perm_changed_event(uuid):
    perm_evt = event_messages_pb2.FilePermChangedEvent()
    perm_evt.file_uuid = uuid

    evt = event_messages_pb2.Event()
    evt.file_perm_changed.CopyFrom(perm_evt)

    return prepare_events([evt])


def prepare_file_renamed_event(uuid):
    top_entry = common_messages_pb2.FileRenamedEntry()
    top_entry.old_uuid = uuid
    top_entry.new_uuid = random_str()
    top_entry.new_parent_uuid = random_str()
    top_entry.new_name = random_str()

    rename_evt = event_messages_pb2.FileRenamedEvent()
    rename_evt.top_entry.CopyFrom(top_entry)

    evt = event_messages_pb2.Event()
    evt.file_renamed.CopyFrom(rename_evt)

    return prepare_events([evt])


def prepare_file_removed_event(uuid):
    remove_evt = event_messages_pb2.FileRemovedEvent()
    remove_evt.file_uuid = uuid

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
        sub = prepare_file_read_subscription(sid)
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
    sub = prepare_file_written_subscription(sid)
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
    with receive(endpoint, msgs_num=2) as queue:
        manager.subscribeFileAttrChanged(uuid, time_thr)
        queue.get() # Skip message stream reset
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_attr_changed')

    sub = client_message.subscription.file_attr_changed
    assert sub.file_uuid == uuid
    assert sub.time_threshold == time_thr


def test_timed_emit_file_attr_changed(endpoint, manager, uuid):
    evt = prepare_file_attr_changed_event(uuid)
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeFileAttrChanged(uuid, 500)

    wait_until(lambda: manager.getHandlerCallsNumber("file_attr_changed") == 1)


def test_cancel_file_attr_changed_subscription(endpoint, manager, uuid):
    with receive(endpoint):
        sid = manager.subscribeFileAttrChanged(uuid, 500)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_location_changed(endpoint, manager, uuid, time_thr):
    with receive(endpoint, msgs_num=2) as queue:
        manager.subscribeFileLocationChanged(uuid, time_thr)
        queue.get() # Skip message stream reset
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_location_changed')

    sub = client_message.subscription.file_location_changed
    assert sub.file_uuid == uuid
    assert sub.time_threshold == time_thr


def test_timed_emit_file_location_changed(endpoint, manager, uuid):
    evt = prepare_file_location_changed_event(uuid)
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeFileLocationChanged(uuid, 500)

    wait_until(lambda: manager.getHandlerCallsNumber("file_location_changed") == 1)


def test_cancel_file_location_changed_subscription(endpoint, manager, uuid):
    with receive(endpoint):
        sid = manager.subscribeFileLocationChanged(uuid, 500)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_perm_changed(endpoint, manager, uuid):
    with receive(endpoint, msgs_num=2) as queue:
        manager.subscribeFilePermChanged(uuid)
        queue.get() # Skip message strip reset
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_perm_changed')

    sub = client_message.subscription.file_perm_changed
    assert sub.file_uuid == uuid


def test_timed_emit_file_perm_changed(endpoint, manager, uuid):
    evt = prepare_file_perm_changed_event(uuid)
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeFilePermChanged(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_perm_changed") == 1)


def test_cancel_file_perm_changed_subscription(endpoint, manager, uuid):
    with receive(endpoint, msgs_num=2):
        sid = manager.subscribeFilePermChanged(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_renamed(endpoint, manager, uuid):
    with receive(endpoint, msgs_num=2) as queue:
        queue.get() # Skip message stream reset
        manager.subscribeFileRenamed(uuid)
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_renamed')

    sub = client_message.subscription.file_renamed
    assert sub.file_uuid == uuid


def test_timed_emit_file_renamed(endpoint, manager, uuid):
    evt = prepare_file_renamed_event(uuid)
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeFileRenamed(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_renamed") == 1)


def test_cancel_file_renamed_subscription(endpoint, manager, uuid):
    with receive(endpoint):
        sid = manager.subscribeFileRenamed(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_file_removed(endpoint, manager, uuid):
    with receive(endpoint, msgs_num=2) as queue:
        manager.subscribeFileRemoved(uuid)
        queue.get() # Skip message stream reset
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('file_removed')

    sub = client_message.subscription.file_removed
    assert sub.file_uuid == uuid


def test_timed_emit_file_removed(endpoint, manager, uuid):
    evt = prepare_file_removed_event(uuid)
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeFileRemoved(uuid)

    wait_until(lambda: manager.getHandlerCallsNumber("file_removed") == 1)


def test_cancel_file_removed_subscription(endpoint, manager, uuid):
    with receive(endpoint):
        sid = manager.subscribeFileRemoved(uuid)

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


def test_subscribe_quota_exceeded(endpoint, manager):
    with receive(endpoint, msgs_num=2) as queue:
        manager.subscribeQuotaExceeded()
        queue.get() # Skip message stream reset
        client_message = queue.get()

    assert client_message.HasField('subscription')
    assert client_message.subscription.HasField('quota_exceeded')


def test_timed_emit_quota_exceeded(endpoint, manager):
    evt = prepare_quota_exceeded_event()
    with reply(endpoint, evt, reply_to_async=True):
        manager.subscribeQuotaExceeded()

    wait_until(lambda: manager.getHandlerCallsNumber("quota_exceeded") == 1)


def test_cancel_quota_exceeded_subscription(endpoint, manager):
    with receive(endpoint):
        sid = manager.subscribeQuotaExceeded()

    with receive(endpoint) as queue:
        assert manager.unsubscribe(sid)
        client_message = queue.get()

    assert client_message.HasField('subscription_cancellation')
    can = client_message.subscription_cancellation
    assert can.id == sid


@pytest.mark.performance(
    repeats=10,
    parameters=[EvtParam.evt_num(10), EvtParam.key_num(5)],
    configs={
        'all': {
            'description': 'All events with the same uuid.',
            'parameters': [EvtParam.evt_num(10000), EvtParam.key_num(1)]
        },
        'half': {
            'description': 'Every second event with the same key.',
            'parameters': [EvtParam.evt_num(10000), EvtParam.key_num(2)]
        },
        'one_tenth': {
            'description': 'Every tenth event with the same key.',
            'parameters': [EvtParam.evt_num(10000), EvtParam.key_num(10)]
        }
    })
def test_aggregate_events(result, endpoint, manager, evt_num, key_num):
    manager.subscribeFileRead(evt_num, -1)
    uuids = [random_str() for _ in range(key_num)]
    evts_per_uuid = evt_num / key_num
    evt_size = 10

    emit_time = Duration()
    with receive(endpoint, msgs_num=2) as queue:
        with measure(emit_time):
            for offset in range(evts_per_uuid):
                for uuid in uuids:
                    manager.emitFileRead(uuid, evt_size * offset, evt_size)
        queue.get() # Skip message stream reset
        client_message = queue.get()

    assert client_message.HasField('events')
    assert len(client_message.events.events) == key_num

    for evt in client_message.events.events:
        assert evt.HasField('file_read')
        assert evt.file_read.counter == evts_per_uuid
        assert evt.file_read.size == evts_per_uuid * evt_size
        assert evt.file_read.file_uuid in uuids
        uuids.remove(evt.file_read.file_uuid)

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
    assert evt.file_uuid == uuid
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
    assert evt.file_uuid == uuid
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
    assert evt.file_uuid == uuid
    assert evt.file_size == file_size
