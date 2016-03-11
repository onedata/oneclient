import copy
import os
import random
import string
import sys
from Queue import Queue
from contextlib import contextmanager
from threading import Thread

import time

_script_dir = os.path.dirname(os.path.realpath(__file__))

# Define variables for use in tests
project_dir = os.path.dirname(os.path.dirname(_script_dir))
appmock_dir = os.path.join(project_dir, 'appmock')
docker_dir = os.path.join(project_dir, 'bamboos', 'docker')
annotations_dir = os.path.join(project_dir, 'test', 'annotations')

# Append useful modules to the path
sys.path = [appmock_dir, docker_dir, annotations_dir] + sys.path

from proto import messages_pb2


def random_int(lower_bound=1, upper_bound=100):
    return random.randint(lower_bound, upper_bound)


def random_str(size=random_int(),
               characters=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(characters) for _ in xrange(size))


def _with_reply_process(endpoint, responses, queue):
    while responses:
        received_msgs = endpoint.wait_for_any_messages(return_history=True, accept_more=True)
        endpoint.client.reset_tcp_history()

        for received_msg in received_msgs:
            client_message = messages_pb2.ClientMessage()
            client_message.ParseFromString(received_msg)

            if hasattr(client_message, 'message_id'):
                response = responses.pop(0)
                response.message_id = client_message.message_id.encode('utf-8')
                endpoint.send(response.SerializeToString())

            queue.put(client_message)


@contextmanager
def reply(endpoint, responses):
    if not isinstance(responses, list):
        responses = [responses]

    queue = Queue()
    p = Thread(target=_with_reply_process, args=(endpoint, responses, queue))
    p.start()

    try:
        yield queue
    finally:
        p.join()


class PerformanceResult(object):
    def __init__(self):
        self.value = []

    def set(self, value):
        self.value = value if isinstance(value, list) else [value]


class Parameter(object):
    """Input/output parameter used by performance test module."""

    def __init__(self, name='', description='', value=0, unit=''):
        self.name = name
        self.description = description
        self.value = value
        self.unit = unit

    def __repr__(self):
        return "Parameter(name: '{0}', description: '{1}', value: {2}," \
               " unit: '{3}')".format(self.name, self.description, self.value,
                                      self.unit)

    def aggregate_value(self, value):
        """Adds given value to the parameter value."""
        self.value += value
        return self

    def append_value(self, rep, value):
        """Appends value of given repeat to the parameter value."""
        self.value.update({str(rep): value})
        return self

    def average(self, n):
        """Returns average parameter value from n repeats."""
        param = copy.copy(self)
        param.value = float(param.value) / n
        return param

    def format(self):
        """Returns parameter fields as a dictionary."""

        try:
            value = round(self.value, 6)
        except TypeError:
            value = self.value

        return {
            'name': self.name,
            'description': self.description,
            'value': value,
            'unit': self.unit
        }

    @staticmethod
    def msg_num(num):
        return Parameter('msg_num', 'Number of sent messages.', num)

    @staticmethod
    def msg_size(size, unit):
        return Parameter('msg_size', 'Size of each message.', size, unit)

    @staticmethod
    def send_time(duration, unit='ms'):
        value = getattr(Duration, unit)(duration)
        return Parameter('send_time',
                         'Summary time since first messages sent '
                         'till last message received.', value, unit)

    @staticmethod
    def recv_time(duration, unit='ms'):
        value = getattr(Duration, unit)(duration)
        return Parameter('recv_time',
                         'Summary time since first reply sent '
                         'till last reply received.', value, unit)

    @staticmethod
    def communicate_time(duration, unit='ms'):
        value = getattr(Duration, unit)(duration)
        return Parameter('communicate_time',
                         'Summary time since first message sent '
                         'till last reply received.', value, unit)

    @staticmethod
    def mbps(msg_num, msg_size, duration):
        return Parameter('mbps', 'Transfer speed.',
                         msg_num * msg_size / 1024. / 1024. / duration.s(),
                         'MB/s')

    @staticmethod
    def msgps(msg_num, duration):
        return Parameter('msgps', 'Messages throughput.',
                         msg_num / duration.s(), 'msg/s')

    def normalized_value(self):
        if self.unit == 'kB':
            return 1024 * self.value
        elif self.unit == 'MB':
            return 1048576 * self.value
        else:
            return self.value


class Duration(object):
    def __init__(self, value=0):
        self.value = value

    def increment(self, microseconds_diff):
        """Increment duration by difference in microseconds."""
        self.value += microseconds_diff

    def us(self):
        """Returns duration in microseconds."""
        return self.value

    def ms(self):
        """Returns duration in milliseconds."""
        return self.value / 1000.

    def s(self):
        """Returns duration in seconds."""
        return self.value / 1000000.


@contextmanager
def measure(duration):
    start_time = time.time()
    yield start_time
    end_time = time.time()
    duration.increment(int((end_time - start_time) * 1000000))
