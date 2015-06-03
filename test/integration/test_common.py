import os
import random
import string
import sys

_script_dir = os.path.dirname(os.path.realpath(__file__))

# Define variables for use in tests
project_dir = os.path.dirname(os.path.dirname(_script_dir))
appmock_dir = os.path.join(project_dir, 'appmock')
docker_dir = os.path.join(project_dir, 'bamboos', 'docker')
annotations_dir = os.path.join(project_dir, 'annotations')

# Append useful modules to the path
sys.path = [appmock_dir, docker_dir, annotations_dir] + sys.path

from performance import Parameter


def random_int():
    return random.randint(1, 100)


def random_str(size=random_int()):
    return ''.join(random.choice(string.ascii_uppercase + string.digits) for
                   _ in xrange(size))


def msg_num(num):
    return Parameter('msg_num', 'Number of messages sent.', num)


def msg_size(size, unit):
    return Parameter('msg_size', 'Size of each message.', size, unit)


def translate_unit(unit):
    if unit == 'kB':
        return 1024
    elif unit == 'MB':
        return 1048576
    else:
        return 1
