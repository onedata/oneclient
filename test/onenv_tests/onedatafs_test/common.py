"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import random
import string
import requests
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

def put_file(oneprovider_host, token, bucket_name, path, data,
             content_type='application/octet-stream'):
    url = (f'https://{oneprovider_host}/api/v3/oneprovider/'
           f'lookup-file-id/{bucket_name}')

    repeats = 5
    space_id = None
    while (repeats > 0) and (space_id is None):
        try:
            r = requests.post(url,
                              headers={'X-Auth-Token': token,
                                       'Content-type': 'application/json'},
                              verify=False)
            if r.status_code != 200:
                repeats -= 1
                continue

            space_id = r.json()["fileId"]
        except Exception as e:
            time.sleep(5)
            repeats -= 1

    url = f'https://{oneprovider_host}/api/v3/oneprovider/' \
          f'data/{space_id}/path/{path}'

    return requests.put(url,
                        data=data,
                        headers={'X-Auth-Token': token,
                                 'Content-type': content_type},
                        verify=False)

