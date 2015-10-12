"""This module tests events emission and subscription using event self.manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
import math
import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))

from test_common import *
from performance import *
from environment import appmock, common, docker

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


class TestEventManager:
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

    @performance({
        'repeats': 10,
        'parameters': [ctr_thr_param(1), evt_num_param(10), evt_size_param(1)],
        'configs': {
            'large_counter_threshold': {
                'description': 'Large counter threshold.',
                'parameters': [ctr_thr_param(100000), evt_num_param(100000)]
            },
            'small_counter_threshold': {
                'description': 'Small counter threshold.',
                'parameters': [ctr_thr_param(10000), evt_num_param(100000)]
            }
        }
    })
    def test_events_emission_when_counter_threshold_exceeded(self, parameters):
        """Test event emission for subscription with counter threshold set."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        ctr_thr = parameters['ctr_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value

        msg = events.createReadEventSubscriptionMsg(1, ctr_thr, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        emit_time = Duration()
        for i in xrange(evt_num):
            duration(emit_time, evt_man.emitReadEvent, i * evt_size, evt_size,
                     'fileUuid')

        recv_time = Duration()
        for i in xrange(evt_num / ctr_thr):
            msg = events.createReadEventMsg(ctr_thr, 'fileUuid', [
                (i * ctr_thr * evt_size, ctr_thr * evt_size)], i)
            duration(recv_time,
                     appmock_client.tcp_server_wait_for_specific_messages,
                     self.ip, 5555, msg)

        return [
            emit_time_param(emit_time.ms()),
            recv_time_param(recv_time.ms()),
            evtps_param(evt_num, emit_time.us() + recv_time.us())
        ]

    @performance({
        'repeats': 10,
        'parameters': [size_thr_param(1), evt_num_param(10), evt_size_param(1)],
        'configs': {
            'large_size_threshold': {
                'description': 'Large size threshold.',
                'parameters': [size_thr_param(100000), evt_num_param(100000)]
            },
            'small_size_threshold': {
                'description': 'Small size threshold.',
                'parameters': [size_thr_param(10000), evt_num_param(100000)]
            }
        }
    })
    def test_events_emission_when_size_threshold_exceeded(self, parameters):
        """Test event emission for subscription with size threshold set."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value
        ctr_thr = int(math.ceil(float(size_thr) / evt_size))

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        emit_time = Duration()
        for i in xrange(evt_num):
            duration(emit_time, evt_man.emitReadEvent, i * evt_size,
                     evt_size, 'fileUuid')

        recv_time = Duration()
        for i in xrange(evt_num * evt_size / size_thr):
            msg = events.createReadEventMsg(ctr_thr, 'fileUuid', [
                (i * ctr_thr * evt_size, ctr_thr * evt_size)], i)
            duration(recv_time,
                     appmock_client.tcp_server_wait_for_specific_messages,
                     self.ip, 5555, msg)

        return [
            emit_time_param(emit_time.ms()),
            recv_time_param(recv_time.ms()),
            evtps_param(evt_num, emit_time.us() + recv_time.us())
        ]

    @performance(skip=True)
    def test_subscription_time_threshold(self, parameters):
        """Test event emission for subscription with time threshold set."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 0, 100, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

    @performance(skip=True)
    def test_multiple_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of the same type."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, 50)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        msg = events.createReadEventSubscriptionMsg(2, 0, 0, 20)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        msg = events.createReadEventSubscriptionMsg(3, 0, 0, 5)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        seq_num = evt_man.emitReadEvent(0, 5, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 5)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createEventSubscriptionCancellationMsg(3)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 20, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 20)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createEventSubscriptionCancellationMsg(2)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 50, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 50)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createEventSubscriptionCancellationMsg(1)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], seq_num)
        with pytest.raises(Exception):
            appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                                 msg,
                                                                 timeout_sec=1)

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
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        cycle_num = parameters['cycle_num'].value
        evt_size = int(math.ceil(float(size_thr) / cycle_num))

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        msg = events.createWriteEventSubscriptionMsg(2, 0, 0, size_thr)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        emit_time = Duration()
        for i in xrange(cycle_num - 1):
            duration(emit_time, evt_man.emitWriteEvent, i * evt_size, evt_size,
                     'fileUuid')
            duration(emit_time, evt_man.emitReadEvent, i * evt_size, evt_size,
                     'fileUuid')
            duration(emit_time, evt_man.emitTruncateEvent, (i + 1) * evt_size,
                     'fileUuid')

        duration(emit_time, evt_man.emitReadEvent, (cycle_num - 1) * evt_size,
                 evt_size, 'fileUuid')

        msg = events.createReadEventMsg(cycle_num, 'fileUuid',
                                        [(0, cycle_num * evt_size)], 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        duration(emit_time, evt_man.emitWriteEvent, (cycle_num - 1) * evt_size,
                 evt_size, 'fileUuid')

        msg = events.createWriteEventMsg(2 * cycle_num - 1, 'fileUuid',
                                         cycle_num * evt_size,
                                         [(0, cycle_num * evt_size)], 1)

        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        return emit_time_param(emit_time.ms())

    @performance(skip=True)
    def test_different_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of a different
        type."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(4, 1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        msg = events.createWriteEventSubscriptionMsg(2, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        seq_num = evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createEventSubscriptionCancellationMsg(1)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        seq_num = evt_man.emitWriteEvent(0, 10, 'fileUuid')
        msg = events.createWriteEventMsg(1, 'fileUuid', 0, [(0, 10)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        seq_num = evt_man.emitTruncateEvent(0, 'fileUuid')
        msg = events.createTruncateEventMsg(1, 'fileUuid', 0, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createEventSubscriptionCancellationMsg(2)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitTruncateEvent(0, 'fileUuid')
        msg = events.createTruncateEventMsg(1, 'fileUuid', 0, seq_num)
        with pytest.raises(Exception):
            appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                                 msg,
                                                                 timeout_sec=1)
