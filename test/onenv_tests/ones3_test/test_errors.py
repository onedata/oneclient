"""Authors: Bartek Kryza
Copyright (C) 2022 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib

import pytest
import botocore
import time
from .common import *


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


def test_error_create_already_existing_bucket(s3_client, bucket):
    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.create_bucket(
            Bucket=bucket,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})

    assert e.value.response['Error']['Code'] == 'BucketAlreadyOwnedByYou'


def test_error_create_bucket_readonly_token(s3_readonly_client):
    bucket = random_str(12)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.create_bucket(
            Bucket=bucket,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_remove_bucket_invalid_bucket(s3_client):
    bucket = random_str()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.delete_bucket(Bucket=bucket)

    assert e.value.response['Error']['Code'] == 'NoSuchBucket'

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-type'] == 'application/xml'


def test_error_remove_bucket_readonly_token(s3_readonly_client, bucket):
    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.delete_bucket(Bucket=bucket)

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_head_bucket_invalid(s3_client):
    bucket = random_str()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.head_bucket(Bucket=bucket)

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-length'] == '0'
    assert 'content-type' not in response_metadata['HTTPHeaders']


def test_error_head_object_invalid_key(s3_client, bucket):
    key = random_str()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.head_object(Bucket=bucket, Key=key)

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-length'] == '0'
    assert 'content-type' not in response_metadata['HTTPHeaders']


def test_error_get_object_invalid_key(s3_client, bucket):
    key = random_str()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.get_object(Bucket=bucket, Key=key)

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-length'] == '0'
    assert 'content-type' not in response_metadata['HTTPHeaders']


def test_error_delete_object_invalid_key(s3_client, bucket):
    key = random_str()

    res = s3_client.delete_object(Bucket=bucket, Key=key)

    assert (res['ResponseMetadata']['HTTPStatusCode'] == 204)


def test_error_delete_objects_invalid_keys(s3_client, bucket):
    files = [f'f-{i}.txt' for i in range(10)]
    objects = {'Objects': [{'Key': f} for f in files]}

    res = s3_client.delete_objects(Bucket=bucket,
                                   Delete=objects)

    assert (res['ResponseMetadata']['HTTPStatusCode'] == 200)
    assert (res['Deleted'] == objects['Objects'])


def test_error_put_object_no_such_bucket(s3_client):
    key = random_str()

    body = random_bytes()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.put_object(Bucket="no_such_bucket", Key=key, Body=body)

    assert e.value.response['Error']['Code'] == 'NoSuchBucket'
    assert e.value.response['Error'][
               'Message'] == 'The specified bucket does not exist'
    assert e.value.response['Error']['BucketName'] == "no_such_bucket"

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-type'] == 'application/xml'


def test_error_put_object_readonly_token(s3_readonly_client, bucket):
    key = random_str()

    body = random_bytes()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.put_object(Bucket=bucket, Key=key, Body=body)

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_put_object_empty_token(s3_readonly_client, bucket):
    body = random_bytes()

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_readonly_client.put_object(Bucket=bucket, Key='__INVALID__',
                                      Body=body)

    assert e.value.response['Error']['Code'] == 'AccessDenied'


def test_error_directory_is_not_object(s3_client, bucket):
    key = 'dir1/dir2/' + random_str()

    body = random_bytes()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        s3_client.get_object(Bucket=bucket, Key='dir1/dir2')

    assert e.value.response['Error']['Code'] == '404'

    response_metadata = e.value.response['ResponseMetadata']
    assert response_metadata['HTTPStatusCode'] == 404
    assert response_metadata['HTTPHeaders']['content-length'] == '0'
    assert 'content-type' not in response_metadata['HTTPHeaders']


def test_error_list_buckets_user_with_no_spaces(s3_client_noone):
    # Make sure multiple calls triggering invalid_provider error during
    # handshake return AccessDenied always
    for _ in range(0, 20):
        with pytest.raises(botocore.exceptions.ClientError) as e:
            s3_client_noone.list_buckets()

        assert (e.value.response['Error']['Code'] == 'AccessDenied') \
               or (e.value.response['Error']['Code'] == '500')


def test_error_list_small_bucket_by_another_user(s3_client, s3_client_joe,
                                                 bucket,
                                                 onezone_admin_token,
                                                 onezone_ip,
                                                 user_joe_id):
    for i in range(20):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i}.txt', Body=b'TEST')

    space_id = get_space_id(onezone_ip, onezone_admin_token, bucket)
    add_user_to_space(onezone_ip, user_joe_id, space_id,
                      ['space_view'])

    # Wait until infer token scope can see that the user was added to the space
    time.sleep(10)

    with pytest.raises(botocore.exceptions.ClientError) as e:
        res = s3_client_joe.list_objects(Bucket=bucket, Delimiter='/',
                                         EncodingType='path', MaxKeys=1000,
                                         Prefix='')

    assert e.value.response['Error']['Code'] == 'AccessDenied'

    remove_user_from_space(onezone_ip, user_joe_id, space_id)
