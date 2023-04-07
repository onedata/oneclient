"""Authors: Bartek Kryza
Copyright (C) 2022 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib

import pytest
import botocore
from concurrent.futures import ThreadPoolExecutor, wait
from .common import random_bytes, random_str


def test_error_list_buckets_invalid_key(s3_client_invalid_key):
    bucket = random_str(12)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client_invalid_key.list_buckets(
            Bucket=bucket,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_create_bucket_invalid_key(s3_client_invalid_key):
    bucket = random_str(12)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client_invalid_key.create_bucket(
            Bucket=bucket,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_put_object_no_such_bucket(s3_client, bucket):
    key = random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    with pytest.raises(Exception) as e:
        s3_client.put_object(Bucket="no_such_bucket", Key=key, Body=body)

    assert str(e.type) == "<class 'botocore.errorfactory.NoSuchBucket'>"


def test_error_directory_is_not_object(s3_client, bucket):
    key = 'dir1/dir2/'+random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    with pytest.raises(Exception) as e:
        s3_client.get_object(Bucket=bucket, Key='dir1/dir2')

    assert str(e.type) == "<class 'botocore.errorfactory.NoSuchKey'>"


@pytest.mark.skip
def test_error_copy_blob_not_implemented(s3_client, bucket):
    source = random_str(12)
    dest = random_str(12)
    body = random_bytes()

    with pytest.raises(Exception) as e:
        s3_client.put_object(Bucket=bucket, Key=source, Body=body)
        s3_client.copy_object(Bucket=bucket, CopySource=f'{bucket}/{source}',
                              Key=dest)

    assert e.value.response['Error']['Code'] == 'NotImplemented'
