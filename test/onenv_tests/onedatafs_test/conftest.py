"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import os
import pytest
import uuid
import requests
import urllib3
import sys

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, script_dir)

import onedatafs

FIXTURE_SCOPE = "session"


@pytest.fixture
def uuid_str():
    yield str(uuid.uuid4())


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_ip():
    ozip = os.getenv('ONEZONE_IP')
    yield ozip


@pytest.fixture(scope=FIXTURE_SCOPE)
def oneprovider_ip():
    opip = os.getenv('ONEPROVIDER_IP')
    yield opip


@pytest.fixture(scope=FIXTURE_SCOPE)
def ceph_monitor_ip():
    cmip = os.getenv('CEPH_MONITOR_IP')
    yield cmip


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_server_ip():
    s3ip = os.getenv('S3_SERVER_IP')
    yield s3ip


@pytest.fixture(scope=FIXTURE_SCOPE)
def onezone_admin_token(onezone_ip):
    tokens_endpoint = f'https://{onezone_ip}/api/v3/onezone/user/client_tokens'
    res = requests.post(tokens_endpoint, {},
                        auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                        verify=False)
    yield res.json()["token"]


@pytest.fixture(scope=FIXTURE_SCOPE)
def ceph_support_storage_id(request, oneprovider_ip, onezone_admin_token):
    storages_endpoint = f'https://{oneprovider_ip}/api/v3/onepanel/provider/storages'
    storages = requests.get(storages_endpoint,
                            headers={'X-Auth-Token': onezone_admin_token},
                            verify=False)
    for storage_id in storages.json()["ids"]:
        storage = requests.get(f'{storages_endpoint}/{storage_id}',
                               headers={'X-Auth-Token': onezone_admin_token},
                               verify=False)
        if 'ceph' in storage.json()['name']:
            yield storage_id


@pytest.fixture(scope=FIXTURE_SCOPE)
def s3_support_storage_id(request, oneprovider_ip, onezone_admin_token):
    storages_endpoint = f'https://{oneprovider_ip}/api/v3/onepanel/provider/storages'
    storages = requests.get(storages_endpoint,
                            headers={'X-Auth-Token': onezone_admin_token},
                            verify=False)
    for storage_id in storages.json()["ids"]:
        storage = requests.get(f'{storages_endpoint}/{storage_id}',
                               headers={'X-Auth-Token': onezone_admin_token},
                               verify=False)
        if 's3' == storage.json()['name']:
            yield storage_id


@pytest.fixture()
def odfs_proxy(oneprovider_ip, onezone_admin_token):
    return onedatafs.OnedataFS(oneprovider_ip, onezone_admin_token,
                               insecure=True, force_proxy_io=True)
