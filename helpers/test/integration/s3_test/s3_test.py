"""This module tests S3 helper."""

__author__ = "Krzysztof Trzepla"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
# noinspection PyUnresolvedReferences
from test_common import *
from environment import docker
from environment import s3 as s3_server
from boto.s3.connection import S3Connection, OrdinaryCallingFormat
from key_value_test_base import *
import s3

BLOCK_SIZE = 100


@pytest.fixture(scope='module')
def client(request):
    class S3Client(object):
        def __init__(self, scheme, host_name, access_key, secret_key, bucket):
            [ip, port] = host_name.split(':')
            self.scheme = scheme
            self.host_name = host_name
            self.access_key = access_key
            self.secret_key = secret_key
            self.bucket = bucket
            self.conn = S3Connection(self.access_key, self.secret_key,
                                     host=ip, port=int(port), is_secure=False,
                                     calling_format=OrdinaryCallingFormat())

        def list(self, file_id):
            bucket = self.conn.get_bucket(self.bucket, validate=False)
            return list(bucket.list(prefix=file_id + '/', delimiter='/'))

    bucket = 'data'
    result = s3_server.up('onedata/s3proxy', [bucket], 'storage', '1')
    [container] = result['docker_ids']

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)

    return S3Client('http', result['host_name'], result['access_key'], result[
        'secret_key'], bucket)


@pytest.fixture
def helper(client):
    return s3.S3Proxy(client.scheme, client.host_name, client.bucket,
                      client.access_key, client.secret_key, BLOCK_SIZE)
