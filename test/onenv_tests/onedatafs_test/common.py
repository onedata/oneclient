"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import random
import string

import time
from contextlib import contextmanager

@contextmanager
def timer() -> float:
    start = time.perf_counter()
    yield lambda: time.perf_counter() - start


def random_int(lower_bound=1, upper_bound=100):
    return random.randint(lower_bound, upper_bound)


def random_str(size=random_int(),
               characters=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(characters) for _ in range(size))


def random_path(size=random_int(3, 10)):
    return '/'.join(random_str(5) for _ in range(size))


def random_bytes(size=random_int()):
    return random_str(size).encode('utf-8')


