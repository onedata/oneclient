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


@pytest.mark.usefixtures("oneclient")
class SpaceOperationsTests(unittest.TestCase):

    def test_space_name_conflict(self):
        """Test space name disambiguation when creating spaces with conflicting names."""

        space_name_1 = 'test_oneclient_conflict_ceph'
        space_name_2 = 'test_oneclient_conflict_ceph_2'

        # Get the space IDs
        space_id_1 = get_space_id(self.onezone_ip, self.onezone_admin_token, space_name_1)
        space_id_2 = get_space_id(self.onezone_ip, self.onezone_admin_token, space_name_2)
        assert space_id_1 is not None, f'Space ID for space {space_name_1} not found'
        assert space_id_2 is not None, f'Space ID for space {space_name_2} not found'

        # Rename the second space to generate conflict
        rename_space(self.onezone_ip, self.onezone_admin_token, space_id_2, space_name_1)

        # Wait a bit for the rename to propagate
        time.sleep(15)

        # List spaces and check for disambiguation
        spaces = [f for f in listdir(self.mountpoint) if isdir(join(self.mountpoint, f))]

        assert 'test_oneclient_conflict_ceph' not in spaces
        assert 'test_oneclient_conflict_ceph_2' not in spaces

        assert f'test_oneclient_conflict_ceph@{space_id_1}' in spaces
        assert f'test_oneclient_conflict_ceph@{space_id_2}' in spaces

        # Rename conflicting space to it's original name
        rename_space(self.onezone_ip, self.onezone_admin_token, space_id_2, space_name_2)

        # Wait a bit for the rename to propagate
        time.sleep(15)

        # List spaces and check for disambiguation
        spaces = [f for f in listdir(self.mountpoint) if isdir(join(self.mountpoint, f))]

        assert 'test_oneclient_conflict_ceph' in spaces
        assert 'test_oneclient_conflict_ceph_2' in spaces

        assert f'test_oneclient_conflict_ceph@{space_id_1}' not in spaces
        assert f'test_oneclient_conflict_ceph@{space_id_2}' not in spaces


@pytest.mark.usefixtures("oneclient")
class ConcurrentXattrOperations(unittest.TestCase):
    """Test parallel xattr operations."""

    space_name = 'test_oneclient_ceph'
    num_files = 250
    num_threads = 25
    operations_per_thread = 25

    def perform_xattr_operations(self, thread_id, file_paths):
        thread_errors = []
        try:
            for _ in range(self.operations_per_thread):
                # Pick a random file
                file_path = random.choice(file_paths)
                xattr_obj = xattr.xattr(file_path)

                # Random operation: set, get, or list
                operation = random.choice(['set', 'get', 'list'])

                if operation == 'set':
                    attr_name = f'user.test_attr_{thread_id}_{random_int(0, 100)}'
                    attr_value = random_bytes(random_int(1, 50))
                    try:
                        xattr_obj[attr_name] = attr_value
                    except OSError as e:
                        if e.errno != errno.ENOTSUP:
                            thread_errors.append(
                                f'Thread {thread_id}: Set xattr failed: {e}')
                elif operation == 'get':
                    try:
                        attrs = list(xattr_obj.keys())
                        if attrs:
                            attr_name = random.choice(attrs)
                            value = xattr_obj.get(attr_name)
                            if value is None:
                                thread_errors.append(
                                    f'Thread {thread_id}: Got None for existing attr {attr_name}')
                    except OSError as e:
                        if e.errno not in (errno.ENODATA, errno.ENOTSUP):
                            thread_errors.append(
                                f'Thread {thread_id}: Get xattr failed: {e}')
                elif operation == 'list':
                    try:
                        attrs = list(xattr_obj.keys())
                        # Basic consistency check - listing should not fail
                        if not isinstance(attrs, list):
                            thread_errors.append(
                                f'Thread {thread_id}: List xattr returned non-list')
                    except OSError as e:
                        if e.errno != errno.ENOTSUP:
                            thread_errors.append(
                                f'Thread {thread_id}: List xattr failed: {e}')

        except Exception as e:
            thread_errors.append(f'Thread {thread_id}: Unexpected error: {e}')

        return thread_errors

    def test_concurrent_xattr_operations(self):
        temp_dir = tempfile.mkdtemp('concurrent_xattr_operations',
                                    dir=f'{self.mountpoint}/{self.space_name}')

        try:
            file_paths = []
            for i in range(self.num_files):
                file_path = os.path.join(temp_dir, f'test_file_{i:03d}.txt')
                with open(file_path, 'wb') as f:
                    f.write(random_bytes(10))
                file_paths.append(file_path)

            errors = []

            # Execute concurrent operations
            with ThreadPoolExecutor(max_workers=self.num_threads) as executor:
                futures = [executor.submit(self.perform_xattr_operations,
                                           i, file_paths)
                          for i in range(self.num_threads)]

                results = wait(futures, return_when=ALL_COMPLETED)

                for future in results.done:
                    thread_errors = future.result()
                    errors.extend(thread_errors)

            # Check for any errors or inconsistencies
            if errors:
                self.fail(f'Concurrent xattr operations had errors:\n' + '\n'.join(errors))

        finally:
            # Cleanup
            try:
                shutil.rmtree(temp_dir)
            except OSError:
                pass


@pytest.mark.usefixtures("oneclient")
class UnpackBagitTest(unittest.TestCase):
    """Test bagit archive unpacking."""
    space_name = 'test_oneclient_ceph'
    test_bagit_url = "http://packages.devel.onedata.org/testdata/test_bagit_unpack.tar"

    def test_unpack_bagit_archive(self):
        temp_dir = tempfile.mkdtemp('bagit_unpack_test_dir',
                                    dir=f'{self.mountpoint}/{self.space_name}')

        try:
            self.unpack_bagit(*self.prepare_env(temp_dir))
        finally:
            self.clean_env(temp_dir)

    def prepare_env(self, root_dir):
        bagit_tar = os.path.join(root_dir, "test_bagit_unpack.tar")
        with requests.get(self.test_bagit_url, stream=True) as r:
            with open(bagit_tar, 'wb') as f:
                shutil.copyfileobj(r.raw, f)

        dst_dir = os.path.join(root_dir, "dst_dir")
        os.mkdir(dst_dir, 0o777)

        return self.get_file_id(bagit_tar), self.get_file_id(dst_dir)

    def get_file_id(self, path):
        file_xattrs = xattr.xattr(path)
        return file_xattrs.get('org.onedata.file_id').decode('utf-8')

    def clean_env(self, root_dir):
        try:
            shutil.rmtree(root_dir)
        except OSError:
            # Already deleted
            pass

    def unpack_file(self, archive, data_dir, dst_dir_path, file_info):
        file_src_path = file_info.name

        if file_src_path.startswith(data_dir) and not file_info.isdir():
            file_data_dir_rel_path = file_src_path[len(data_dir) :].lstrip("/")

            # Adjust the file path so that when unpacking only parent directories
            # up to data directory will be created in destination directory
            # (normally all directories on path are created)
            file_info.name = file_data_dir_rel_path
            archive.extract(file_info, dst_dir_path)

            file_path = f"{dst_dir_path}/{file_data_dir_rel_path}"

            self.assertEqual(file_info.size, os.path.getsize(file_path))

    def unpack_bagit(self, archive_file_id, dst_dir_file_id):
        archive_path = f"{self.mountpoint}/.__onedata__file_id__{archive_file_id}"
        dst_dir_path = f"{self.mountpoint}/.__onedata__file_id__{dst_dir_file_id}"

        members = tarfile.TarFile(archive_path)

        def unpack_file_wrap(file_info):
            with tarfile.TarFile(archive_path) as archive:
                data_dir = f"{self.get_bagit_dir_name(archive)}/data"
                return self.unpack_file(
                    archive, data_dir, dst_dir_path, file_info)

        start_time = time.time()

        workers_count = 8

        with ThreadPoolExecutor(max_workers=workers_count) as executor:
            executor.map(unpack_file_wrap, members)

        print(f'Bagit unpack took {time.time() - start_time}'
              f' using {workers_count} threads')

    def get_bagit_dir_name(self, archive):
        for path in archive.getnames():
            path_tokens = path.split("/")
            if len(path_tokens) == 2 and path_tokens[1] == "bagit.txt":
                return path_tokens[0]
