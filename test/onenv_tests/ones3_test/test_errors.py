"""Authors: Bartek Kryza
Copyright (C) 2022 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib

import pytest
import botocore
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


def test_error_create_bucket_readonly_token(s3_readonly_client):
    bucket = random_str(12)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.create_bucket(
            Bucket=bucket,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_remove_bucket_readonly_token(s3_readonly_client, bucket):
    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.delete_bucket(Bucket=bucket)

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_put_object_no_such_bucket(s3_client):
    key = random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    with pytest.raises(Exception) as e:
        s3_client.put_object(Bucket="no_such_bucket", Key=key, Body=body)

    assert str(e.type) == "<class 'botocore.errorfactory.NoSuchBucket'>"


def test_error_put_object_readonly_token(s3_readonly_client, bucket):
    key = random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    with pytest.raises(Exception) as e:
        s3_readonly_client.put_object(Bucket=bucket, Key=key, Body=body)

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_directory_is_not_object(s3_client, bucket):
    key = 'dir1/dir2/'+random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    with pytest.raises(Exception) as e:
        s3_client.get_object(Bucket=bucket, Key='dir1/dir2')

    assert str(e.type) == "<class 'botocore.errorfactory.NoSuchKey'>"
