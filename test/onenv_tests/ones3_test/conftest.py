"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import os
import random
import string

import pytest
from botocore.config import Config
import botocore
import boto3
import uuid
import requests
import subprocess
import time
import configparser
import logging

from functools import wraps

#
# Uncomment to trace boto3 requests
#
# boto3.set_stream_logger('', logging.DEBUG)

FIXTURE_SCOPE = "session"


@pytest.fixture(scope=FIXTURE_SCOPE)
def git_version():
    gv = os.getenv('GIT_VERSION')
    yield gv


@pytest.fixture
def uuid_str():
    return str(uuid.uuid4())


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_ip():
    ozip = os.getenv('ONEZONE_IP')
    yield ozip


@pytest.fixture(scope=FIXTURE_SCOPE)
def oneprovider_ip():
    opip = os.getenv('ONEPROVIDER_IP')
    yield opip


@pytest.fixture(scope=FIXTURE_SCOPE)
def oneprovider_2_ip():
    opip = os.getenv('ONEPROVIDER_2_IP')
    yield opip


@pytest.fixture(scope=FIXTURE_SCOPE)
def redis_ip():
    rip = os.getenv('REDIS_IP')
    yield rip


@pytest.fixture(scope=FIXTURE_SCOPE)
def ceph_monitor_ip():
    cmip = os.getenv('CEPH_MONITOR_IP')
    yield cmip


@pytest.fixture(scope=FIXTURE_SCOPE)
def user_joe_id(onezone_ip):
    user_endpoint = f'https://{onezone_ip}/api/v3/onezone/user'
    res = requests.get(user_endpoint,
                       auth=requests.auth.HTTPBasicAuth('joe', 'password'),
                       verify=False)
    return res.json()["userId"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_admin_token(onezone_ip):
    tokens_endpoint = f'https://{onezone_ip}/api/v3/onezone/user/client_tokens'
    res = requests.post(tokens_endpoint, {},
                        auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                        verify=False)
    return res.json()["token"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_joe_token(onezone_ip):
    tokens_endpoint = f'https://{onezone_ip}/api/v3/onezone/user/client_tokens'
    res = requests.post(tokens_endpoint, {},
                        auth=requests.auth.HTTPBasicAuth('joe', 'password'),
                        verify=False)
    return res.json()["token"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_noone_token(onezone_ip):
    tokens_endpoint = f'https://{onezone_ip}/api/v3/onezone/user/client_tokens'
    res = requests.post(tokens_endpoint, {},
                        auth=requests.auth.HTTPBasicAuth('noone', 'password'),
                        verify=False)
    return res.json()["token"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_readonly_token(onezone_ip):
    """Generate new readonly only client token."""
    temporary_token_path = 'api/v3/onezone/user/tokens/temporary'
    tokens_endpoint = f'https://{onezone_ip}/{temporary_token_path}'
    headers = {'content-type': 'application/json'}
    res = requests.post(tokens_endpoint,
                        json={
                            "type": {
                                "accessToken": {}
                            },
                            "caveats": [{
                                "type": "data.readonly"
                            }, {
                                "type":
                                    "time",
                                "validUntil":
                                    int(time.time()) + 2592000
                            }]
                        },
                        headers=headers,
                        auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                        verify=False)
    return res.json()["token"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def secret_access_key():
    return 'TestSecretAccessKey'


@pytest.fixture(scope=FIXTURE_SCOPE)
def readonly_secret_access_key():
    return 'ReadOnlyAccessKey'


@pytest.fixture(scope=FIXTURE_SCOPE, params=['ceph'])
def support_storage_id(request, oneprovider_ip, onezone_admin_token):
    storages_endpoint = f'https://{oneprovider_ip}/api/v3/onepanel/provider/storages'
    storages = requests.get(storages_endpoint,
                            headers={'X-Auth-Token': onezone_admin_token},
                            verify=False)
    for storage_id in storages.json()["ids"]:
        storage = requests.get(f'{storages_endpoint}/{storage_id}',
                               headers={'X-Auth-Token': onezone_admin_token},
                               verify=False)
        if request.param in storage.json()['name']:
            return storage_id


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_host():
    return '0.0.0.0'


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_port():
    return '8080'


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_endpoint(s3_host, s3_port):
    return f'http://{s3_host}:{s3_port}'


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_server(request, onezone_ip, oneprovider_ip, ceph_monitor_ip,
              onezone_admin_token,
              support_storage_id, s3_port):
    ones3_cli = (
        f'debug/s3/ones3'
        f' --custom-ca-dir test/onenv_tests/certs'
        f' -v 1'
        f' --onezone-host dev-onezone.default.svc.cluster.local'
        f' -H dev-oneprovider-krakow.default.svc.cluster.local'
        f' --ones3-support-storage-id {support_storage_id}'
        f' --ones3-support-storage-credentials onepanel:password'
        f' --override {support_storage_id}:monitorHostname:{ceph_monitor_ip}'
        f' --ones3-thread-num 10 --scheduler-thread-count 1 --storage-helper-thread-count 10'
        f' --ones3-http-port {s3_port} --force-direct-io --no-buffer --provider-timeout 180')
    proc = subprocess.Popen(ones3_cli.split(' '))
    print(f"-- Starting ones3 server: {ones3_cli}")
    time.sleep(15)
    print("-- Done")
    request.addfinalizer(proc.kill)


def create_s3client(s3_endpoint, access_token, secret_key):
    s3_config = Config(
        region_name='pl-reg-k1',
        connect_timeout=120,
        read_timeout=300,
        signature_version='s3v4',
        retries={
            'max_attempts': 3,
            'mode': 'standard'
        }
    )

    return boto3.client(
        service_name='s3',
        endpoint_url=os.getenv('AWS_S3_ENDPOINT', s3_endpoint),
        verify=False,
        region_name="pl-reg-k1",
        config=s3_config,
        aws_access_key_id=access_token,
        aws_secret_access_key=secret_key
    )


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_static_client(onezone_admin_token, s3_server, secret_access_key,
                     s3_endpoint):
    return create_s3client(s3_endpoint, onezone_admin_token, secret_access_key)


@pytest.fixture(scope=FIXTURE_SCOPE)
def dummy_bucket(s3_static_client):
    uuid_str = "dummy_test_bucket"
    try:
        s3_static_client.create_bucket(Bucket=uuid_str,
                                       CreateBucketConfiguration={
                                           'LocationConstraint': 'pl-reg-k1'})
    except botocore.exceptions.ClientError as e:
        pass
    yield uuid_str


@pytest.fixture
def s3_client(onezone_admin_token, s3_server, secret_access_key, s3_endpoint):
    return create_s3client(s3_endpoint, onezone_admin_token, secret_access_key)


@pytest.fixture
def s3_readonly_client(s3_server, onezone_readonly_token,
                       readonly_secret_access_key, s3_endpoint):
    return create_s3client(s3_endpoint, onezone_readonly_token,
                           readonly_secret_access_key)


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_client_invalid_key(s3_server, s3_endpoint):
    return create_s3client(s3_endpoint, 'INVALID_KEY_ID', 'INVALID_SECRET')


@pytest.fixture
def s3_client_joe(onezone_joe_token, s3_server, secret_access_key, s3_endpoint):
    return create_s3client(s3_endpoint, onezone_joe_token, secret_access_key)


@pytest.fixture
def s3_client_noone(onezone_noone_token, s3_server, secret_access_key, s3_endpoint):
    return create_s3client(s3_endpoint, onezone_noone_token, secret_access_key)


@pytest.fixture(scope=FIXTURE_SCOPE)
def rclone_setup(onezone_admin_token, secret_access_key, s3_endpoint):
    name = 's3proxy'

    config_path = os.path.expanduser("~/.config/rclone/rclone.conf")

    config = configparser.ConfigParser()
    if os.path.exists(config_path):
        config.read(config_path)

    if name not in config:
        config[name] = {}

    config[name]["type"] = "s3"
    config[name]["access_key_id"] = onezone_admin_token
    config[name]["secret_access_key"] = secret_access_key
    config[name]["endpoint"] = s3_endpoint

    if "provider" not in config[name]:
        config[name]["provider"] = "Other"
    if "env_auth" not in config[name]:
        config[name]["env_auth"] = "false"
    if "location_constraint" not in config[name]:
        config[name]["location_constraint"] = "us-east-1"
    if "acl" not in config[name]:
        config[name]["acl"] = "private"
    if "bucket_acl" not in config[name]:
        config[name]["bucket_acl"] = "private"
    if "upload_cutoff" not in config[name]:
        config[name]["upload_cutoff"] = "20M"
    if "chunk_size" not in config[name]:
        config[name]["chunk_size"] = "16M"
    if "multi-thread-streams" not in config[name]:
        config[name]["multi-thread-streams"] = "20"

    os.makedirs(os.path.dirname(config_path), exist_ok=True)

    with open(config_path, 'w') as configfile:
        config.write(configfile)


@pytest.fixture(scope=FIXTURE_SCOPE)
def minio_setup(onezone_admin_token, secret_access_key, s3_server, s3_endpoint):
    os.system(
        f'mc alias set --insecure s3proxy "{s3_endpoint}" {onezone_admin_token} {secret_access_key}')


def create_bucket(s3_client, uuid_str):
    with pytest.raises(Exception) as e:
        s3_client.create_bucket(Bucket=uuid_str,
                                CreateBucketConfiguration={
                                    'LocationConstraint': 'pl-reg-k1'})
        if str(e.type) == "<class 'botocore.errorfactory.BucketAlreadyOwnedByYou'>":
            pass
        else:
            raise e


def clean_bucket(s3_client, name, prefix=''):
    continuation_token = ''
    while True:
        res = s3_client.list_objects_v2(Bucket=name, Delimiter='',
                                        EncodingType='url', MaxKeys=1000,
                                        Prefix=prefix,
                                        ContinuationToken=continuation_token)
        if 'CommonPrefixes' in res:
            for cp in res['CommonPrefixes']:
                p = cp['Prefix']
                clean_bucket(s3_client, name, p)

        if 'Contents' in res:
            objects = []
            for k in res['Contents']:
                key = k['Key']
                objects.append({'Key': key})

            s3_client.delete_objects(Bucket=name, Delete={'Objects': objects})

        if res['IsTruncated']:
            continuation_token = res['NextContinuationToken']
        else:
            break


def delete_bucket(s3_client, uuid_str):
    s3_client.delete_bucket(Bucket=uuid_str)


@pytest.fixture
def bucket(s3_client, uuid_str):

    create_bucket(s3_client, uuid_str)

    yield uuid_str

    clean_bucket(s3_client, uuid_str)

    delete_bucket(s3_client, uuid_str)
