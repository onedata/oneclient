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
    """Returns parameter that describes number of events per second."""
    return Parameter('evtps', 'Number of events per second.',
                     1000000. * evt_num / us, 'event/s')


def wait_for_result(expected, attempts, delay_ms, function, *args):
    """Executes function periodically as long as it does not return expected
    value or number of attemptes is exceeded."""
    while True:
        if attempts <= 0:
            return False
        else:
            result = function(*args)
            if result == expected:
                return True
            else:
                attempts -= 1
                time.sleep(delay_ms / 1000.)

class TestEventManager:
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

    def subscribe(self, evt_man, id, msg):
        """Ensures that client receives subscription."""
        wait_for_result(True, 10, 500, self.send_subscription, evt_man, id, msg)

    def send_subscription(self, evt_man, id, msg):
        """Sends subscription to the client and checks whether it has been received."""
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        return wait_for_result(True, 20, 500, evt_man.existSubscription, id)

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
        evt_man = events.EventManager(1, self.ip, 5555)

        ctr_thr = parameters['ctr_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value

        msg = events.createReadEventSubscriptionMsg(1, ctr_thr, 0, 0)
        self.subscribe(evt_man, 1, msg)

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
        evt_man = events.EventManager(1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value
        ctr_thr = int(math.ceil(float(size_thr) / evt_size))

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, size_thr)
        self.subscribe(evt_man, 1, msg)

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
    def test_events_emission_when_time_threshold_exceeded(self, parameters):
        """Test event emission for subscription with time threshold set."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 0, 100, 0)
        self.subscribe(evt_man, 1, msg)
        evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

    @performance(skip=True)
    def test_multiple_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of the same type."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, 50)
        self.subscribe(evt_man, 1, msg)
        msg = events.createReadEventSubscriptionMsg(2, 0, 0, 20)
        self.subscribe(evt_man, 2, msg)
        msg = events.createReadEventSubscriptionMsg(3, 0, 0, 5)
        self.subscribe(evt_man, 3, msg)

        seq_num = evt_man.emitReadEvent(0, 5, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 5)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createServerSubscriptionCancellationMsg(3, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 20, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 20)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createServerSubscriptionCancellationMsg(2, 0, 1)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 50, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 50)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createServerSubscriptionCancellationMsg(1, 0, 2)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], seq_num)
        with pytest.raises(Exception):
            appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                                 msg,
                                                                 timeout_sec=1)

    @performance({
        'repeats': 10,
        'parameters': [size_thr_param(100), cycle_num_param(2)],
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
        evt_man = events.EventManager(1, self.ip, 5555)

        size_thr = parameters['size_thr'].value
        cycle_num = parameters['cycle_num'].value
        evt_size = int(math.ceil(float(size_thr) / cycle_num))

        msg = events.createReadEventSubscriptionMsg(1, 0, 0, size_thr)
        self.subscribe(evt_man, 1, msg)
        msg = events.createWriteEventSubscriptionMsg(2, 0, 0, size_thr)
        self.subscribe(evt_man, 2, msg)

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
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg,
                                                             timeout_sec=30)

        duration(emit_time, evt_man.emitWriteEvent, (cycle_num - 1) * evt_size,
                 evt_size, 'fileUuid')

        msg = events.createWriteEventMsg(2 * cycle_num - 1, 'fileUuid',
                                         cycle_num * evt_size,
                                         [(0, cycle_num * evt_size)], 0)

        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg,
                                                             timeout_sec=30)

        return emit_time_param(emit_time.ms())

    @performance(skip=True)
    def test_different_subscriptions(self, parameters):
        """Test event emission for multiple subscriptions of a different
        type."""
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        msg = events.createReadEventSubscriptionMsg(1, 1, 0, 0)
        self.subscribe(evt_man, 1, msg)
        msg = events.createWriteEventSubscriptionMsg(2, 1, 0, 0)
        self.subscribe(evt_man, 2, msg)

        seq_num = evt_man.emitReadEvent(0, 10, 'fileUuid')
        msg = events.createReadEventMsg(1, 'fileUuid', [(0, 10)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createServerSubscriptionCancellationMsg(1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)

        seq_num = evt_man.emitWriteEvent(0, 10, 'fileUuid')
        msg = events.createWriteEventMsg(1, 'fileUuid', 0, [(0, 10)], seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        seq_num = evt_man.emitTruncateEvent(0, 'fileUuid')
        msg = events.createTruncateEventMsg(1, 'fileUuid', 0, seq_num)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

        msg = events.createServerSubscriptionCancellationMsg(2, 1, 0)
        appmock_client.tcp_server_send(self.ip, 5555, msg)
        seq_num = evt_man.emitTruncateEvent(0, 'fileUuid')
        msg = events.createTruncateEventMsg(1, 'fileUuid', 0, seq_num)
        with pytest.raises(Exception):
            appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555,
                                                                 msg,
                                                                 timeout_sec=1)

    @performance(skip=True)
    def test_file_attr_subscription(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        id = evt_man.subscribeFileAttr('fileUuid', 1, 10, 2, 5)
        assert id < 0

        msg = events.createFileAttrSubscriptionMsg(id, 'fileUuid', 2, 5, 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

    @performance(skip=True)
    def test_file_attr_subscription_cancellation(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        id = evt_man.subscribeFileAttr('fileUuid', 1, 10, 2, 5)
        evt_man.unsubscribe(id)

        msg = events.createClientSubscriptionCancellationMsg(id, 2, 1)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

    @performance(skip=True)
    def test_file_attr_counter_emission(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        evt_man.subscribeFileAttr('fileUuid', 10, 5000, 1, 1000)

        assert 0 == evt_man.fileAttrHandlerCallCounter()

        for i in xrange(100):
            msg = events.createFileAttrEventMsg(1, 'fileUuid', i, i)
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        assert 10 == evt_man.fileAttrHandlerCallCounter()

    @performance(skip=True)
    def test_file_attr_time_emission(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        evt_man.subscribeFileAttr('fileUuid', 100, 1000, 1, 1000)

        assert 0 == evt_man.fileAttrHandlerCallCounter()

        for i in xrange(10):
            msg = events.createFileAttrEventMsg(1, 'fileUuid', i, i)
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        assert wait_for_result(1, 20, 500, evt_man.fileAttrHandlerCallCounter)

    @performance(skip=True)
    def test_file_location_subscription(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        id = evt_man.subscribeFileLocation('fileUuid', 1, 10, 2, 5)
        msg = events.createFileLocationSubscriptionMsg(id, 'fileUuid', 2, 5, 0)
        appmock_client.tcp_server_wait_for_specific_messages(self.ip, 5555, msg)

    @performance(skip=True)
    def test_file_location_counter_emission(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        evt_man.subscribeFileLocation('fileUuid', 20, 5000, 2, 5)

        assert 0 == evt_man.fileLocationHandlerCallCounter()

        for i in xrange(100):
            msg = events.createFileLocationEventMsg(1, 'fileUuid', 'fileId', i)
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        assert 5 == evt_man.fileLocationHandlerCallCounter()

    @performance(skip=True)
    def test_file_location_time_emission(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        evt_man = events.EventManager(1, self.ip, 5555)

        evt_man.subscribeFileLocation('fileUuid', 100, 1000, 2, 5)

        assert 0 == evt_man.fileLocationHandlerCallCounter()

        for i in xrange(10):
            msg = events.createFileLocationEventMsg(1, 'fileUuid', 'fileId', i)
            appmock_client.tcp_server_send(self.ip, 5555, msg)

        assert wait_for_result(1, 20, 500,
                               evt_man.fileLocationHandlerCallCounter)
