"""This module tests events emission and subscription using event self.manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import math
import time

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
from test_common import *
from performance import *

# noinspection PyUnresolvedReferences
from environment import appmock, common, docker
# noinspection PyUnresolvedReferences
import events
import appmock_client


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


# noinspection PyClassHasNoInit
class TestCommunicator:
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
        self.manager = events.EventManager(1, self.ip, 5555)

    def teardown_method(self, _):
        del self.manager

    @performance({
        'repeats': 10,
        'parameters': [ctr_thr_param(1), evt_num_param(10), evt_size_param(1)],
        'configs': {
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
        }
    })
    def test_subscription_counter_threshold(self, parameters):
        """Test event emission for subscription with counter threshold set."""
        self.manager = events.EventManager(1, self.ip, 5555)

        ctr_thr = parameters['ctr_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value

        sub = events.prepareSerializedReadEventSubscription(1, ctr_thr, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        emit_time = Duration()
        for i in xrange(evt_num):
            duration(emit_time, self.manager.emitReadEvent, 'fileId',
                     i * evt_size,
                     evt_size)

        recv_time = Duration()
        for i in xrange(evt_num / ctr_thr):
            evt = events.prepareSerializedReadEvent(ctr_thr, 'fileId',
                                                    i * ctr_thr * evt_size,
                                                    ctr_thr * evt_size, i)
            duration(recv_time,
                     appmock_client.tcp_server_wait_for_specific_messages,
                     self.ip, 5555, evt, 1, False, 50)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_specific_message_count(self.ip,
                                                                     5555, evt)

        return [
            emit_time_param(emit_time.ms()),
            recv_time_param(recv_time.ms()),
            evtps_param(evt_num, emit_time.us() + recv_time.us())
        ]

    @performance({
        'repeats': 10,
        'parameters': [size_thr_param(2), evt_num_param(10), evt_size_param(1)],
        'configs': {
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
        }
    })
    def test_subscription_size_threshold(self, parameters):
        """Test event emission for subscription with size threshold set."""
        self.manager = events.EventManager(1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value
        ctr_thr = int(math.ceil(float(size_thr) / evt_size))

        sub = events.prepareSerializedReadEventSubscription(1, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        emit_time = Duration()
        for i in xrange(evt_num):
            duration(emit_time, self.manager.emitReadEvent, 'fileId',
                     i * evt_size,
                     evt_size)

        recv_time = Duration()
        for i in xrange(evt_num * evt_size / size_thr):
            evt = events.prepareSerializedReadEvent(ctr_thr, 'fileId',
                                                    i * ctr_thr * evt_size,
                                                    ctr_thr * evt_size, i)
            duration(recv_time,
                     appmock_client.tcp_server_wait_for_specific_messages,
                     self.ip, 5555, evt, 1, False, 10)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, size_thr)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_specific_message_count(self.ip,
                                                                     5555, evt)

        return [
            emit_time_param(emit_time.ms()),
            recv_time_param(recv_time.ms()),
            evtps_param(evt_num, emit_time.us() + recv_time.us())
        ]

    @performance(skip=True)
    def test_subscription_time_threshold(self, parameters):
        """Test event emission for subscription with time threshold set."""
        self.manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 0, 100, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        self.manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_specific_message_count(self.ip,
                                                                     5555, evt)

    @performance(skip=True)
    def test_multiple_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of the same type."""
        self.manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 0, 0, 50)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedReadEventSubscription(2, 0, 0, 20)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedReadEventSubscription(3, 0, 0, 5)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        seq_num = self.manager.emitReadEvent('fileId', 0, 5)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 5, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(3)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, 20)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 20, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(2)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, 50)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 50, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_specific_message_count(self.ip,
                                                                     5555, evt)

    @performance({
        'repeats': 10,
        'parameters': [size_thr_param(100), cycle_num_param(10)],
        'configs': {
            'multiple_events': {
                'description': 'Aggregates multiple events.',
                'parameters': [size_thr_param(1000000),
                               cycle_num_param(1000000)]
            }
        }
    })
    def test_write_read_truncate_event_aggregation(self, parameters):
        """Test aggregation of write, read and truncate events."""
        self.manager = events.EventManager(1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        cycle_num = parameters['cycle_num'].value
        evt_size = int(math.ceil(float(size_thr) / cycle_num))

        sub = events.prepareSerializedReadEventSubscription(1, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedWriteEventSubscription(2, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        emit_time = Duration()
        for i in xrange(cycle_num - 1):
            duration(emit_time, self.manager.emitWriteEvent, 'fileId',
                     i * evt_size,
                     evt_size)
            duration(emit_time, self.manager.emitReadEvent, 'fileId',
                     i * evt_size,
                     evt_size)
            duration(emit_time, self.manager.emitTruncateEvent, 'fileId',
                     (i + 1) * evt_size)

        duration(emit_time, self.manager.emitReadEvent, 'fileId',
                 (cycle_num - 1)
                 * evt_size, evt_size)

        evt = events.prepareSerializedReadEvent(cycle_num, 'fileId', 0,
                                                cycle_num * evt_size, 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt,
                                                             timeout_sec=30)

        duration(emit_time, self.manager.emitWriteEvent, 'fileId',
                 (cycle_num - 1)
                 * evt_size, evt_size)

        evt = events.prepareSerializedWriteTruncatedEvent(2 * cycle_num - 1,
                                                          'fileId', 0,
                                                          cycle_num * evt_size,
                                                          cycle_num * evt_size,
                                                          1)

        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt,
                                                             timeout_sec=30)

        return emit_time_param(emit_time.ms())

    @performance(skip=True)
    def test_different_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of a different
         type."""
        self.manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedWriteEventSubscription(2, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        seq_num = self.manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)

        seq_num = self.manager.emitWriteEvent('fileId', 0, 10)
        evt = events.prepareSerializedWriteEvent(1, 'fileId', 0, 10, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        seq_num = self.manager.emitTruncateEvent('fileId', 0)
        evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, evt)

        can = events.prepareSerializedEventSubscriptionCancellation(2)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = self.manager.emitTruncateEvent('fileId', 0)
        evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_specific_message_count(self.ip,
                                                                     5555, evt)
