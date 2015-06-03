"""This module tests events emission and sub using evt manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2015 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
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


def ctr_thr(value):
    return Parameter('ctr_thr', 'Counter threshold.', value)


def evt_num(value):
    return Parameter('evt_num', 'Number of emitted events.', value)


def evt_size(value):
    return Parameter('evt_size', 'Size of each evt.', value)


# noinspection PyClassHasNoInit
class TestCommunicator:
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
        'parameters': [ctr_thr(1), evt_num(10), evt_size(1)],
        'configs': {
            'multiple_small_events': {
                'description': 'Emits multiple small events using evt'
                               'manager.',
                'parameters': [ctr_thr(100), evt_num(100000), evt_size(10)]
            },
            'multiple_large_events': {
                'description': 'Emits multiple large events using evt'
                               'manager.',
                'parameters': [ctr_thr(100), evt_num(100000), evt_size(1000)]
            }
        }
    })
    def test_subscription_counter_threshold(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        manager = events.EventManager(1, self.ip, 5555)

        ctr_thr = parameters['ctr_thr'].value
        evt_num = parameters['evt_num'].value
        evt_size = parameters['evt_size'].value

        sub = events.prepareSerializedReadEventSubscription(1, ctr_thr, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        emit_time = Duration()
        for i in xrange(evt_num):
            duration(emit_time, manager.emitReadEvent, 'fileId', i * evt_size,
                     evt_size)

        for i in xrange(evt_num / ctr_thr):
            evt = events.prepareSerializedReadEvent(ctr_thr, 'fileId',
                                                    i * ctr_thr * evt_size,
                                                    ctr_thr * evt_size, i)
            appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1,
                                                        5)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555, evt)

        return Parameter('emit_time', 'Summary events emission time.',
                         emit_time.ms(), 'ms')

    @performance(skip=True)
    def test_subscription_size_threshold(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 0, 0, 10)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, 0)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555, evt)

    @performance(skip=True)
    def test_subscription_time_threshold(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 0, 100, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, 0)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555, evt)

    @performance(skip=True)
    def test_multiple_subscriptions(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 0, 0, 50)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedReadEventSubscription(2, 0, 0, 20)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedReadEventSubscription(3, 0, 0, 5)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        seq_num = manager.emitReadEvent('fileId', 0, 5)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 5, seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(3)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 20)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 20, seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(2)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 50)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 50, seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555, evt)

    @performance(skip=True)
    def test_different_subscriptions(self, parameters):
        appmock_client.reset_tcp_server_history(self.ip)
        manager = events.EventManager(1, self.ip, 5555)

        sub = events.prepareSerializedReadEventSubscription(1, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)
        sub = events.prepareSerializedWriteEventSubscription(2, 1, 0, 0)
        appmock_client.tcp_server_send(self.ip, 5555, sub)

        seq_num = manager.emitReadEvent('fileId', 0, 10)
        evt = events.prepareSerializedReadEvent(1, 'fileId', 0, 10, seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(1)
        appmock_client.tcp_server_send(self.ip, 5555, can)

        seq_num = manager.emitWriteEvent('fileId', 0, 10, 10)
        evt = events.prepareSerializedWriteEvent(1, 'fileId', 0, 10, 10,
                                                 seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        seq_num = manager.emitTruncateEvent('fileId', 0)
        evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
        appmock_client.tcp_server_wait_for_messages(self.ip, 5555, evt, 1, 5)

        can = events.prepareSerializedEventSubscriptionCancellation(2)
        appmock_client.tcp_server_send(self.ip, 5555, can)
        seq_num = manager.emitTruncateEvent('fileId', 0)
        evt = events.prepareSerializedTruncateEvent(1, 'fileId', 0, seq_num)
        time.sleep(0.5)
        assert 0 == appmock_client.tcp_server_message_count(self.ip, 5555, evt)
