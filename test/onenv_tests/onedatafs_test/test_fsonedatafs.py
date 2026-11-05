"""Authors: Bartek Kryza
Copyright (C) 2025 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import unittest
import random
import string
import time
import math
import os
import pytest
import sys
import pytz

from datetime import datetime

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, script_dir)

from fs.test import FSTestCases
from fs.onedatafs._onedatafs import OnedataFS


@pytest.fixture(scope="class")
def onedatafs_instance(request, oneprovider_ip, onezone_admin_token):
    odfs = OnedataFS(oneprovider_ip, onezone_admin_token,
                         insecure=True,
                         force_proxy_io=True, no_buffer=False,
                         provider_timeout=120).opendir('/test_fsonedatafs')

    request.cls.odfs = odfs
    yield odfs
    try:
        odfs.close()
    except:
        pass

@pytest.mark.usefixtures("onedatafs_instance")
class TestOnedataFS(FSTestCases, unittest.TestCase):
    def make_fs(self):
        testdir = ''.join(
            random.choice(string.ascii_lowercase) for _ in range(16))
        self.odfs.makedir(testdir)
        return self.odfs.opendir(testdir)

    def destroy_fs(self, fs):
        pass

    def test_openbin_truncate(self):
        self.fs.writetext('foo', u'abcd')
        self.fs.openbin('foo', 'w').close()
        self.assertEqual(self.fs.getsize('foo'), 0)

    def test_setinfo_size(self):
        self.fs.writetext('foo', u'abcd')
        self.fs.setinfo('foo', {'details': {'size': 1}})
        self.assertEqual(self.fs.getsize('foo'), 1)

    def test_setinfo_atime(self):
        self.fs.writetext('foo', u'abcd')
        now = int(math.floor(time.time())) + 3600
        now_datetime = datetime.utcfromtimestamp(now).replace(tzinfo=pytz.UTC)
        self.fs.setinfo('foo', {'details': {'accessed': now}})
        self.assertEqual(
            self.fs.getinfo('foo', namespaces=['details']).accessed,
            now_datetime)

    def test_setinfo_mtime(self):
        self.fs.writetext('foo', u'abcd')
        now = int(math.floor(time.time()) + 3600)
        now_datetime = datetime.utcfromtimestamp(now).replace(tzinfo=pytz.UTC)
        self.fs.setinfo('foo', {'details': {'modified': now}})
        self.assertEqual(
            self.fs.getinfo('foo', namespaces=['details']).modified,
            now_datetime)

    def test_setinfo_mode(self):
        self.fs.writetext('foo', u'abcd')
        self.fs.setinfo('foo', {'access': {'permissions': [u'g_r', u'o_x', u'u_r', u'u_w']}})
        self.assertEqual(self.fs.getinfo('foo', namespaces=['access']).permissions.as_str(), u'rw-r----x')
