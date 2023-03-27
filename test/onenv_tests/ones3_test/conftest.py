"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import os
import random
import string

import pytest
from botocore.config import Config
import boto3
import uuid
import requests
import subprocess
import time

import logging
#
# Uncomment to trace boto3 requests
#
# boto3.set_stream_logger('', logging.DEBUG)


def clean_bucket(s3_client, name, prefix=''):
    continuation_token = ''
    while True:
        res = s3_client.list_objects_v2(Bucket=name, Delimiter='/',
                EncodingType='url', MaxKeys=1000, Prefix=prefix,
                ContinuationToken=continuation_token)
        if 'CommonPrefixes' in res:
            for cp in res['CommonPrefixes']:
                p = cp['Prefix']
                clean_bucket(s3_client, name, p)

        if 'Contents' in res:
            for k in res['Contents']:
                key = k['Key']
                s3_client.delete_object(Bucket=name, Key=key)

        if res['IsTruncated']:
            continuation_token = res['NextContinuationToken']
        else:
            break


@pytest.fixture
def uuid_str():
    return str(uuid.uuid4())



@pytest.fixture(scope="module")
def onezone_ip():
    ozip = os.getenv('ONEZONE_IP')
    yield ozip



@pytest.fixture(scope="module")
def oneprovider_ip():
    opip = os.getenv('ONEPROVIDER_IP')
    yield opip



@pytest.fixture(scope="module")
def ceph_monitor_ip():
    cmip = os.getenv('CEPH_MONITOR_IP')
    yield cmip


@pytest.fixture(scope="module")
def onezone_admin_token(onezone_ip):
    tokens_endpoint = f'https://{onezone_ip}/api/v3/onezone/user/client_tokens'
    res = requests.post(tokens_endpoint, {},
            auth=requests.auth.HTTPBasicAuth('admin', 'password'), verify=False)
    return res.json()["token"]


@pytest.fixture(scope="module")
def support_storage_id(oneprovider_ip, onezone_admin_token):
    storages_endpoint = f'https://{oneprovider_ip}/api/v3/onepanel/provider/storages'
    storages = requests.get(storages_endpoint,
                        headers={'X-Auth-Token': onezone_admin_token}, verify=False)
    for storage_id in storages.json()["ids"]:
        storage = requests.get(f'{storages_endpoint}/{storage_id}',
                           headers={'X-Auth-Token': onezone_admin_token}, verify=False)
        if 'ceph' in storage.json()['name']:
            return storage_id


@pytest.fixture(scope="module")
def ones3_server(request, onezone_ip, oneprovider_ip, ceph_monitor_ip, onezone_admin_token,
                 support_storage_id):
    ones3_cli = (
            f'debug/s3/ones3 -i -v 1 --onezone-host {onezone_ip} -H {oneprovider_ip}'
            f' -t {onezone_admin_token} --ones3-support-storage-id {support_storage_id}'
            f' --override {support_storage_id}:monitorHostname:{ceph_monitor_ip}'
            f' --ones3-http-port 8081 --force-direct-io --no-buffer')
    proc = subprocess.Popen(ones3_cli.split(' '))
    print("-- Starting ones3 server: {ones3-cli}")
    time.sleep(5)
    print("-- Done")
    request.addfinalizer(proc.kill)


@pytest.fixture
def s3_client(onezone_admin_token, ones3_server):
    s3_config = Config(
        region_name='pl-reg-k1', signature_version='s3v4',
        retries={
            'max_attempts': 0,
            'mode': 'standard'
        }
    )

    return boto3.client(
        service_name='s3',
        endpoint_url=os.getenv('AWS_S3_ENDPOINT', 'http://0.0.0.0:8081'),
        verify=False,
        region_name="pl-reg-k1",
        config=s3_config,
        aws_access_key_id=onezone_admin_token,
        aws_secret_access_key='SecretKey'
    )


@pytest.fixture(scope="module")
def s3_client_invalid_key(ones3_server):
    s3_config = Config(
        region_name='pl-reg-k1', signature_version='s3v4',
        retries={
            'max_attempts': 0,
            'mode': 'standard'
        }
    )

    return boto3.client(
        service_name='s3',
        endpoint_url=os.getenv('AWS_S3_ENDPOINT', 'http://0.0.0.0:8081'),
        verify=False,
        region_name="pl-reg-k1",
        config=s3_config,
        aws_access_key_id='INVALID_KEY_ID',
        aws_secret_access_key='INVALID_SECRET'
    )


@pytest.fixture
def bucket(s3_client, uuid_str):
    s3_client.create_bucket(Bucket=uuid_str,
            CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})
    yield uuid_str
    clean_bucket(s3_client, uuid_str)
    s3_client.delete_bucket(Bucket=uuid_str)
