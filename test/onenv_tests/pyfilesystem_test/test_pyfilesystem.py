"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

from __future__ import unicode_literals

import sys

import errno
import io
import os
import shutil
import tempfile
import time
import unittest
import warnings
import zipfile
import pytest

from six import text_type

import fs
from fs import zipfs
from fs.compress import write_zip
from fs import errors, open_fs, osfs
from fs.path import dirname, relpath
from fs.test import FSTestCases

try:
    from unittest import mock
except ImportError:
    import mock

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

@pytest.mark.usefixtures("oneclient")
class WriteZipFSS3Test(FSTestCases, unittest.TestCase):
    """
    Test ZIPFS implementation.

    When writing, a ZipFS is essentially a TempFS.

    """
    space_name = 'test_pyfilesystem_s3'

    def make_fs(self):
        _zip_file = tempfile.TemporaryFile(
            dir=f'{self.mountpoint}/{self.space_name}')
        fs = zipfs.ZipFS(_zip_file, write=True)
        fs._zip_file = _zip_file
        return fs

    def destroy_fs(self, fs):
        fs.close()
        del fs._zip_file


@pytest.mark.usefixtures("oneclient")
class WriteZipFSCephTest(FSTestCases, unittest.TestCase):
    """
    Test ZIPFS implementation.

    When writing, a ZipFS is essentially a TempFS.

    """
    space_name = 'test_pyfilesystem_ceph'

    def make_fs(self):
        _zip_file = tempfile.TemporaryFile(
            dir=f'{self.mountpoint}/{self.space_name}')
        fs = zipfs.ZipFS(_zip_file, write=True)
        fs._zip_file = _zip_file
        return fs

    def destroy_fs(self, fs):
        fs.close()
        del fs._zip_file


class OSFSBase(FSTestCases):
    """Test OSFS implementation."""

    @classmethod
    def setUpClass(cls):
        warnings.simplefilter("error")

    @classmethod
    def tearDownClass(cls):
        warnings.simplefilter(warnings.defaultaction)


    def _get_real_path(self, path):
        _path = os.path.join(self.fs.root_path, relpath(path))
        return _path

    def assert_exists(self, path):
        _path = self._get_real_path(path)
        self.assertTrue(os.path.exists(_path))

    def assert_not_exists(self, path):
        _path = self._get_real_path(path)
        self.assertFalse(os.path.exists(_path))

    def assert_isfile(self, path):
        _path = self._get_real_path(path)
        self.assertTrue(os.path.isfile(_path))

    def assert_isdir(self, path):
        _path = self._get_real_path(path)
        self.assertTrue(os.path.isdir(_path))

    def assert_bytes(self, path, contents):
        assert isinstance(contents, bytes)
        _path = self._get_real_path(path)
        with io.open(_path, "rb") as f:
            data = f.read()
        self.assertEqual(data, contents)
        self.assertIsInstance(data, bytes)

    def assert_text(self, path, contents):
        assert isinstance(contents, text_type)
        _path = self._get_real_path(path)
        with io.open(_path, "rt", encoding="utf-8") as f:
            data = f.read()
        self.assertEqual(data, contents)
        self.assertIsInstance(data, text_type)

    def test_not_exists(self):
        with self.assertRaises(errors.CreateFailed):
            osfs.OSFS("/does/not/exists/")

    def test_expand_vars(self):
        self.fs.makedir("TYRIONLANISTER")
        self.fs.makedir("$FOO")
        path = self.fs.getsyspath("$FOO")
        os.environ["FOO"] = "TYRIONLANISTER"
        fs1 = osfs.OSFS(path)
        fs2 = osfs.OSFS(path, expand_vars=False)
        self.assertIn("TYRIONLANISTER", fs1.getsyspath("/"))
        self.assertNotIn("TYRIONLANISTER", fs2.getsyspath("/"))

    def test_copy_preserve_time(self):
        self.fs.makedir("foo")
        self.fs.makedir("bar")
        self.fs.create("foo/file.txt")
        raw_info = {"details": {"modified": time.time() - 10000}}
        self.fs.setinfo("foo/file.txt", raw_info)

        namespaces = ("details", "modified")
        src_info = self.fs.getinfo("foo/file.txt", namespaces)

        self.fs.copy("foo/file.txt", "bar/file.txt", preserve_time=True)
        self.assertTrue(self.fs.exists("bar/file.txt"))

        dst_info = self.fs.getinfo("bar/file.txt", namespaces)
        delta = dst_info.modified - src_info.modified
        self.assertAlmostEqual(delta.total_seconds(), 0, places=0)

    @unittest.skipUnless(osfs.sendfile, "sendfile not supported")
    @unittest.skipIf(
        sys.version_info >= (3, 8),
        "the copy function uses sendfile in Python 3.8+, "
        "making the patched implementation irrelevant",
        )
    def test_copy_sendfile(self):
        # try copying using sendfile
        with mock.patch.object(osfs, "sendfile") as sendfile:
            sendfile.side_effect = OSError(errno.ENOSYS, "sendfile not supported")
            self.test_copy()
        # check other errors are transmitted
        self.fs.touch("foo")
        with mock.patch.object(osfs, "sendfile") as sendfile:
            sendfile.side_effect = OSError(errno.EWOULDBLOCK)
            with self.assertRaises(OSError):
                self.fs.copy("foo", "foo_copy")
        # check parent exist and is dir
        with self.assertRaises(errors.ResourceNotFound):
            self.fs.copy("foo", "spam/eggs")
        with self.assertRaises(errors.DirectoryExpected):
            self.fs.copy("foo", "foo_copy/foo")

    def test_create(self):
        """Test create=True"""

        dir_path = tempfile.mkdtemp()
        try:
            create_dir = os.path.join(dir_path, "test_create")
            with osfs.OSFS(create_dir, create=True):
                self.assertTrue(os.path.isdir(create_dir))
            self.assertTrue(os.path.isdir(create_dir))
        finally:
            shutil.rmtree(dir_path)

        # Test exception when unable to create dir
        with tempfile.NamedTemporaryFile() as tmp_file:
            with self.assertRaises(errors.CreateFailed):
                # Trying to create a dir that exists as a file
                osfs.OSFS(tmp_file.name, create=True)

    def test_unicode_paths(self):
        dir_path = tempfile.mkdtemp()
        try:
            fs_dir = os.path.join(dir_path, "te\u0161t_\u00fanicod\u0113")
            os.mkdir(fs_dir)
            with osfs.OSFS(fs_dir):
                self.assertTrue(os.path.isdir(fs_dir))
        finally:
            shutil.rmtree(dir_path)

    @unittest.skipUnless(hasattr(os, "symlink"), "No symlink support")
    def test_symlinks(self):
        with open(self._get_real_path("foo"), "wb") as f:
            f.write(b"foobar")
        os.symlink(self._get_real_path("foo"), self._get_real_path("bar"))
        self.assertFalse(self.fs.islink("foo"))
        self.assertFalse(self.fs.getinfo("foo", namespaces=["link"]).is_link)
        self.assertTrue(self.fs.islink("bar"))
        self.assertTrue(self.fs.getinfo("bar", namespaces=["link"]).is_link)

        foo_info = self.fs.getinfo("foo", namespaces=["link", "lstat"])
        self.assertIn("link", foo_info.raw)
        self.assertIn("lstat", foo_info.raw)
        self.assertEqual(foo_info.get("link", "target"), None)
        self.assertEqual(foo_info.target, foo_info.raw["link"]["target"])
        bar_info = self.fs.getinfo("bar", namespaces=["link", "lstat"])
        self.assertIn("link", bar_info.raw)
        self.assertIn("lstat", bar_info.raw)

    def test_setinfo(self):
        self.fs.create("birthday.txt")
        now = time.time()

        change_info = {"details": {"accessed": now + 60, "modified": now + 60 * 60}}
        self.fs.setinfo("birthday.txt", change_info)
        new_info = self.fs.getinfo("birthday.txt", namespaces=["details"])
        can_write_acccess = new_info.is_writeable("details", "accessed")
        can_write_modified = new_info.is_writeable("details", "modified")
        if can_write_acccess:
            self.assertAlmostEqual(
                new_info.get("details", "accessed"), now + 60, delta=2
            )
        if can_write_modified:
            self.assertAlmostEqual(
                new_info.get("details", "modified"), now + 60 * 60, delta=2
            )

        with self.assertRaises(errors.ResourceNotFound):
            self.fs.setinfo("nothing", {})

    def test_validatepath(self):
        """Check validatepath detects bad encodings."""

        with mock.patch("fs.osfs.fsencode") as fsencode:
            fsencode.side_effect = lambda error: "–".encode("ascii")
            with self.assertRaises(errors.InvalidCharsInPath):
                with self.fs.open("13 – Marked Register.pdf", "wb") as fh:
                    fh.write(b"foo")

    def test_consume_geturl(self):
        self.fs.create("foo")
        try:
            url = self.fs.geturl("foo", purpose="fs")
        except errors.NoURL:
            self.assertFalse(self.fs.hasurl("foo"))
        else:
            self.assertTrue(self.fs.hasurl("foo"))

        # Should not throw an error
        base_dir = dirname(url)
        open_fs(base_dir)

    def test_complex_geturl(self):
        self.fs.makedirs("foo/bar ha")
        test_fixtures = [
            # test file, expected url path
            ["foo", "foo"],
            ["foo-bar", "foo-bar"],
            ["foo_bar", "foo_bar"],
            ["foo/bar ha/barz", "foo/bar%20ha/barz"],
            ["example b.txt", "example%20b.txt"],
            ["exampleㄓ.txt", "example%E3%84%93.txt"],
        ]
        file_uri_prefix = "osfs://"
        for test_file, relative_url_path in test_fixtures:
            self.fs.create(test_file)
            expected = file_uri_prefix + self.fs.getsyspath(relative_url_path).replace(
                "\\", "/"
            )
            actual = self.fs.geturl(test_file, purpose="fs")

            self.assertEqual(actual, expected)

    def test_geturl_return_no_url(self):
        self.assertRaises(errors.NoURL, self.fs.geturl, "test/path", "upload")


@pytest.mark.usefixtures("oneclient")
class OSFSS3Test(OSFSBase, FSTestCases, unittest.TestCase):
    """Test OSFS implementation."""
    space_name = 'test_pyfilesystem_s3'

    def make_fs(self):
        temp_dir = tempfile.mkdtemp('pyfs_test',
                                    dir=f'{self.mountpoint}/{self.space_name}')
        return osfs.OSFS(temp_dir)

    def destroy_fs(self, fs):
        self.fs.close()
        try:
            shutil.rmtree(fs.getsyspath("/"))
        except OSError:
            # Already deleted
            pass


@pytest.mark.usefixtures("oneclient")
class OSFSCephTest(OSFSBase, FSTestCases, unittest.TestCase):
    """Test OSFS implementation."""
    space_name = 'test_pyfilesystem_ceph'

    def make_fs(self):
        temp_dir = tempfile.mkdtemp('pyfs_test',
                                    dir=f'{self.mountpoint}/{self.space_name}')
        return osfs.OSFS(temp_dir)

    def destroy_fs(self, fs):
        self.fs.close()
        try:
            shutil.rmtree(fs.getsyspath("/"))
        except OSError:
            # Already deleted
            pass
