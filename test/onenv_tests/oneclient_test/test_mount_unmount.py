"""Authors: Bartek Kryza
Copyright (C) 2025 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

from __future__ import unicode_literals

import sys

import errno
import io
import os
import requests
import shutil
import tarfile
import tempfile
import time
import unittest
import warnings
import zipfile
import xattr
import pytest
import hashlib
import random
import subprocess

from six import text_type

from os import listdir
from os.path import isdir, join
from concurrent.futures import ThreadPoolExecutor, wait, ALL_COMPLETED
from .common import random_bytes, random_str, random_int, timer
from .common import get_space_id, rename_space

try:
    from unittest import mock
except ImportError:
    import mock

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

class OneclientMountTimeout(Exception): pass

def wait_until(duration, condition):
    wait_end = time.time() + duration
    while not condition():
        if time.time() > wait_end:
            raise OneclientMountTimeout
        time.sleep(1)

def test_mount_unmount(request, onezone_ip, oneprovider_ip, ceph_monitor_ip,
                       onezone_admin_token, ceph_support_storage_id,
                       s3_support_storage_id, s3_server_ip, mountpoint):

    for i in range(1, 10):
        print(f'Mount/unmount iteration {i}')

        oneclient_cli = (
            f'debug/oneclient -v 2 -f'
            f' -H dev-oneprovider-krakow.default.svc.cluster.local'
            f' --custom-ca-dir test/onenv_tests/certs'
            f' --message-trace-log'
            f' -t {onezone_admin_token}'
            f' --override {ceph_support_storage_id}:monitorHostname:{ceph_monitor_ip}'
            f' --override {s3_support_storage_id}:hostname:{s3_server_ip}:9000'
            f' --scheduler-thread-count 1 --storage-helper-thread-count 10'
            f' --force-proxy-io {mountpoint}')
        proc = subprocess.Popen(oneclient_cli.split(' '))
        print(f"-- Starting oneclient: {oneclient_cli}")
        wait_until(30,
                   lambda: os.path.exists(f'{mountpoint}/.__onedata_mountpoint__'))
        print("-- Done")

        def unmount():
            print(f"-- Stopping oneclient")

            proc.kill()
            unmount_cli = f'fusermount3 -uz {mountpoint}'
            subprocess.Popen(unmount_cli.split(' '))

        test_file = f'{mountpoint}/test_oneclient_ceph/testtt.txt'
        with open(test_file, "w") as file:
            file.write("TEST")

        os.remove(test_file)

        unmount()
