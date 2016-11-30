"""This module tests events emission and subscription using event manager."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import math
import os
import sys
import time

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))

from test_common import *
import events

@pytest.fixture
def endpoint(appmock_client):
    return appmock_client.tcp_endpoint(5555)


@pytest.fixture
def evt_man(endpoint):
    return events.EventManager(endpoint.ip, endpoint.port)


def test_read_event_emission(endpoint, evt_man):
    pass
