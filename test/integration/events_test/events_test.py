"""This module tests events emission and subscription using event self.manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import math
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
import events


def ctr_thr_param(value):
    """Returns counter threshold parameter."""
    return Parameter('ctr_thr', 'Counter threshold.', value)


def size_thr_param(value):
    """Returns size threshold parameter."""
    return Parameter('size_thr', 'Size threshold.', value)


def evt_num_param(value):
    """Returns event number parameter."""
    return Parameter('evt_num', 'Number of emitted events.', value)


def evt_size_param(value):
    """Returns event size parameter."""
    return Parameter('evt_size', 'Size of each event.', value, 'B')


def cycle_num_param(value):
    """Returns parameter that describes number of write-read-truncate event
    cycles."""
    return Parameter('cycle_num', 'Number of write-read-truncate event cycles.',
                     value)


def emit_time_param(value, unit='ms'):
    """Returns parameter that describes summary events emission time."""
    return Parameter('emit_time', 'Summary events emission time.', value, unit)


def recv_time_param(value, unit='ms'):
    """Returns parameter that describes summary events emission time."""
    return Parameter('recv_time', 'Summary time to receive all aggregated event'
                                  ' messages.', value, unit)


def evtps_param(evt_num, us):
    return Parameter('evtps', 'Number of events per second.',
                     1000000. * evt_num / us, 'event/s')


@pytest.fixture
def endpoint(appmock_client):
    return appmock_client.tcp_endpoint(5555)


@pytest.fixture
def manager(endpoint):
    return events.EventManager(1, endpoint.ip, endpoint.port)


@pytest.mark.performance(
    repeats=10,
    parameters=[ctr_thr_param(1), evt_num_param(10), evt_size_param(1)],
    configs={
        'multiple_small_events': {
            'description': 'Emits multiple small events using event '
                           'self.manager.',
            'parameters': [ctr_thr_param(100), evt_num_param(100000),
                           evt_size_param(10)]
        },
        'multiple_large_events': {
            'description': 'Emits multiple large events using event '
                           'self.manager.',
            'parameters': [ctr_thr_param(100), evt_num_param(100000),
                           evt_size_param(1000)]
        }
    })
def test_subscription_counter_threshold(result, ctr_thr, evt_num, evt_size,
                                        endpoint,
                                        manager):
    """Test event emission for subscription with counter threshold set."""

    sub = events.prepareSerializedReadEventSubscription(1, ctr_thr, 0, 0)
    endpoint.send(sub)

    emit_time = Duration()
    for i in xrange(evt_num):
        with measure(emit_time):
            manager.emitReadEvent('fileId', i * evt_size, evt_size)

    recv_time = Duration()
    for i in xrange(evt_num / ctr_thr):
        evt = events.prepareSerializedReadEvent(ctr_thr, 'fileId',
                                                i * ctr_thr * evt_size,
                                                ctr_thr * evt_size, i)

        with measure(recv_time):
            endpoint.wait_for_specific_messages(evt, timeout_sec=50)

    can = events.prepareSerializedEventSubscriptionCancellation(1)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, 10)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
    time.sleep(0.5)
    assert 0 == endpoint.specific_message_count(evt)

    result.set([
        emit_time_param(emit_time.ms()),
        recv_time_param(recv_time.ms()),
        evtps_param(evt_num, emit_time.us() + recv_time.us())
    ])


@pytest.mark.performance(
    repeats=10,
    parameters=[size_thr_param(2), evt_num_param(10), evt_size_param(1)],
    configs={
        'multiple_small_events': {
            'description': 'Emits multiple small events using event '
                           'self.manager.',
            'parameters': [size_thr_param(100), evt_num_param(100000),
                           evt_size_param(1)]
        },
        'multiple_large_events': {
            'description': 'Emits multiple large events using event '
                           'self.manager.',
            'parameters': [size_thr_param(10000), evt_num_param(100000),
                           evt_size_param(100)]
        }
    })
def test_subscription_size_threshold(result, size_thr, evt_num, evt_size,
                                     endpoint,
                                     manager):
    """Test event emission for subscription with size threshold set."""

    ctr_thr = int(math.ceil(float(size_thr) / evt_size))

    sub = events.prepareSerializedReadEventSubscription(1, 0, 0, size_thr)
    endpoint.send(sub)

    emit_time = Duration()
    for i in xrange(evt_num):
        with measure(emit_time):
            manager.emitReadEvent('fileId', i * evt_size, evt_size)

    recv_time = Duration()
    for i in xrange(evt_num * evt_size / size_thr):
        evt = events.prepareSerializedReadEvent(ctr_thr, 'fileId',
                                                i * ctr_thr * evt_size,
                                                ctr_thr * evt_size, i)

        with measure(recv_time):
            endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(1)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, size_thr)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
    time.sleep(0.5)
    assert 0 == endpoint.specific_message_count(evt)

    result.set([
        emit_time_param(emit_time.ms()),
        recv_time_param(recv_time.ms()),
        evtps_param(evt_num, emit_time.us() + recv_time.us())
    ])


def test_subscription_time_threshold(endpoint, manager):
    """Test event emission for subscription with time threshold set."""

    sub = events.prepareSerializedReadEventSubscription(1, 0, 100, 0)
    endpoint.send(sub)
    manager.emitReadEvent('fileId', 0, 10)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, 0)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(1)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, 10)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
    time.sleep(0.5)

    assert 0 == endpoint.specific_message_count(evt)


def test_multiple_subscriptions(endpoint, manager):
    """Test event emission for multiple subscriptions of the same type."""

    sub = events.prepareSerializedReadEventSubscription(1, 0, 0, 50)
    endpoint.send(sub)
    sub = events.prepareSerializedReadEventSubscription(2, 0, 0, 20)
    endpoint.send(sub)
    sub = events.prepareSerializedReadEventSubscription(3, 0, 0, 5)
    endpoint.send(sub)

    seq_num = manager.emitReadEvent('fileId', 0, 5)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 5, seq_num)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(3)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, 20)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 20, seq_num)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(2)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, 50)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 50, seq_num)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(1)
    endpoint.send(can)
    seq_num = manager.emitReadEvent('fileId', 0, 10)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
    time.sleep(0.5)

    assert 0 == endpoint.specific_message_count(evt)


@pytest.mark.performance(
    repeats=10,
    parameters=[size_thr_param(100), cycle_num_param(10)],
    configs={
        'multiple_events': {
            'description': 'Aggregates multiple events.',
            'parameters': [size_thr_param(1000000),
                           cycle_num_param(1000000)]
        }
    })
def test_write_read_truncate_event_aggregation(result, size_thr, cycle_num,
                                               endpoint,
                                               manager):
    """Test aggregation of write, read and truncate events."""

    evt_size = int(math.ceil(float(size_thr) / cycle_num))

    sub = events.prepareSerializedReadEventSubscription(1, 0, 0, size_thr)
    endpoint.send(sub)
    sub = events.prepareSerializedWriteEventSubscription(2, 0, 0, size_thr)
    endpoint.send(sub)

    emit_time = Duration()
    for i in xrange(cycle_num - 1):
        with measure(emit_time):
            manager.emitWriteEvent('fileId', i * evt_size, evt_size)
            manager.emitReadEvent('fileId', i * evt_size, evt_size)
            manager.emitTruncateEvent('fileId', (i + 1) * evt_size)

    with measure(emit_time):
        manager.emitReadEvent('fileId', (cycle_num - 1) * evt_size, evt_size)

    evt = events.prepareSerializedReadEvent(cycle_num, 'fileId', 0,
                                            cycle_num * evt_size, 0)
    endpoint.wait_for_specific_messages(evt, timeout_sec=30)

    with measure(emit_time):
        manager.emitWriteEvent('fileId', (cycle_num - 1) * evt_size, evt_size)

    evt = events.prepareSerializedWriteTruncatedEvent(2 * cycle_num - 1,
                                                      'fileId', 0,
                                                      cycle_num * evt_size,
                                                      cycle_num * evt_size,
                                                      1)

    endpoint.wait_for_specific_messages(evt, timeout_sec=30)

    result.set(emit_time_param(emit_time.ms()))


def test_different_subscriptions(endpoint, manager):
    """Test event emission for multiple subscriptions of a different
     type."""

    sub = events.prepareSerializedReadEventSubscription(1, 1, 0, 0)
    endpoint.send(sub)
    sub = events.prepareSerializedWriteEventSubscription(2, 1, 0, 0)
    endpoint.send(sub)

    seq_num = manager.emitReadEvent('fileId', 0, 10)
    evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(1)
    endpoint.send(can)

    seq_num = manager.emitWriteEvent('fileId', 0, 10)
    evt = events.prepareSerializedWriteEvent(1, 'fileId', 0, 10, seq_num)
    endpoint.wait_for_specific_messages(evt)

    seq_num = manager.emitTruncateEvent('fileId', 0)
    evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
    endpoint.wait_for_specific_messages(evt)

    can = events.prepareSerializedEventSubscriptionCancellation(2)
    endpoint.send(can)
    seq_num = manager.emitTruncateEvent('fileId', 0)
    evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
    time.sleep(0.5)
    assert 0 == endpoint.specific_message_count(evt)
