"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import os

import pytest
import uuid
import requests
import subprocess
import time
from pathlib import Path

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

FIXTURE_SCOPE = "session"

class OneclientMountTimeout(Exception): pass

def wait_until(duration, condition):
    wait_end = time.time() + duration
    while not condition():
        if time.time() > wait_end:
            raise OneclientMountTimeout
        time.sleep(1)


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
def mountpoint():
    mount_path = "/tmp/oneclientmnt"
    Path(mount_path).mkdir(parents=True, exist_ok=True)
    yield mount_path


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


@pytest.fixture(scope="class")
def oneclient(request, oneprovider_ip, ceph_monitor_ip, onezone_admin_token,
              ceph_support_storage_id, s3_support_storage_id, s3_server_ip,
              mountpoint):
    oneclient_cli = (
        f'debug/oneclient -i -v 1 -f -H {oneprovider_ip}'
        f' -t {onezone_admin_token}'
        f' --override {ceph_support_storage_id}:monitorHostname:{ceph_monitor_ip}'
        f' --override {s3_support_storage_id}:hostname:{s3_server_ip}:9000'
        f' --scheduler-thread-count 1 --storage-helper-thread-count 10'
        f' --force-direct-io {mountpoint}')
    proc = subprocess.Popen(oneclient_cli.split(' '))
    print(f"-- Starting oneclient: {oneclient_cli}")
    wait_until(30, lambda: os.path.exists(f'{mountpoint}/test_pyfilesystem_ceph'))
    print("-- Done")

    def unmount():
        print(f"-- Stopping oneclient")

        proc.kill()
        time.sleep(5)
        unmount_cli = f'fusermount3 -uz {mountpoint}'
        subprocess.Popen(unmount_cli.split(' '))

    request.addfinalizer(unmount)

    request.cls.mountpoint = mountpoint


@pytest.fixture(scope="class")
def oneclient_proxy(request, oneprovider_ip, ceph_monitor_ip, onezone_admin_token,
              ceph_support_storage_id, s3_support_storage_id, s3_server_ip,
              mountpoint):
    oneclient_cli = (
        f'debug/oneclient -i -v 1 -f -H {oneprovider_ip}'
        f' -t {onezone_admin_token}'
        f' --communicator-thread-count 10 --scheduler-thread-count 1'
        f' --storage-helper-thread-count 10'
        f' --force-proxy-io {mountpoint}')
    proc = subprocess.Popen(oneclient_cli.split(' '))
    print(f"-- Starting oneclient: {oneclient_cli}")
    wait_until(30, lambda: os.path.exists(f'{mountpoint}/test_pyfilesystem_ceph'))
    print("-- Done")

    def unmount():
        print(f"-- Stopping oneclient")

        proc.kill()
        time.sleep(5)
        unmount_cli = f'fusermount3 -uz {mountpoint}'
        subprocess.Popen(unmount_cli.split(' '))

    request.addfinalizer(unmount)

    request.cls.mountpoint = mountpoint
