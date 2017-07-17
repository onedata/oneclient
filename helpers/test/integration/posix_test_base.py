"""This module contains test for POSIX based helpers."""

__author__ = "Bartek Kryza"
__copyright__ = """(C) 2017 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

from test_common import *
from common_test_base import *
from posix_test_types import *
import pytest


@pytest.fixture
def mountpoint(server):
    return server.mountpoint


@pytest.mark.readwrite_operations_tests
def test_read_should_read_written_data(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    assert helper.read(file_id, offset, len(data)) == data


@pytest.mark.readwrite_operations_tests
def test_read_should_error_file_not_found(helper, file_id):
    offset = random_int()
    size = random_int()

    with pytest.raises(RuntimeError) as excinfo:
        helper.read(file_id, offset, size)
    assert 'No such file or directory' in str(excinfo.value)


@pytest.mark.directory_operations_tests
def test_mkdir_should_create_directory(helper, file_id):
    dir_id = random_str()
    data = random_str()
    offset = random_int()

    try:
        helper.mkdir(dir_id, 0777)
    except:
        fail("Couldn't create directory: %s"%(dir_id))

    assert helper.write(dir_id+"/"+file_id, data, offset) == len(data)


@pytest.mark.directory_operations_tests
def test_rename_directory_should_rename(helper, file_id):
    dir1_id = random_str()
    dir2_id = random_str()
    data = random_str()
    offset = random_int()

    helper.mkdir(dir1_id, 0777)
    helper.rename(dir1_id, dir2_id)

    assert helper.write(dir2_id+"/"+file_id, data, offset) == len(data)


@pytest.mark.directory_operations_tests
def test_readdir_should_list_files_in_directory(helper):
    dir_id = random_str()
    file1_id = random_str()
    file2_id = random_str()
    data = random_str()
    offset = random_int()

    try:
        helper.mkdir(dir_id, 0777)
        helper.write(dir_id+"/"+file1_id, data, offset)
        helper.write(dir_id+"/"+file2_id, data, offset)
    except:
        fail("Couldn't create directory: %s"%(dir_id))

    assert len(helper.readdir(dir_id, 0, 1024)) == 2
    assert file1_id in helper.readdir(dir_id, 0, 1024)
    assert file2_id in helper.readdir(dir_id, 0, 1024)


@pytest.mark.directory_operations_tests
def test_rmdir_should_remove_directory(helper):
    dir_id = random_str()
    file1_id = random_str()
    file2_id = random_str()
    data = random_str()
    offset = random_int()

    try:
        helper.mkdir(dir_id, 0777)
        helper.write(dir_id+"/"+file1_id, data, offset)
        helper.write(dir_id+"/"+file2_id, data, offset)
    except:
        fail("Couldn't create directory: %s"%(dir_id))

    with pytest.raises(RuntimeError) as excinfo:
        helper.rmdir(dir_id)
    assert 'Directory not empty' in str(excinfo.value)

    helper.unlink(dir_id+"/"+file1_id)
    helper.unlink(dir_id+"/"+file2_id)

    with pytest.raises(RuntimeError) as excinfo:
        helper.read(dir_id+"/"+file1_id, offset, len(data))
    assert 'No such file or directory' in str(excinfo.value)

    helper.rmdir(dir_id)

    with pytest.raises(RuntimeError) as excinfo:
        helper.readdir(dir_id, 0, 1024)
    assert 'No such file or directory' in str(excinfo.value)


@pytest.mark.remove_operations_tests
def test_unlink_should_pass_errors(helper, file_id):

    with pytest.raises(RuntimeError) as excinfo:
        helper.unlink(file_id)
    assert 'No such file or directory' in str(excinfo.value)


@pytest.mark.remove_operations_tests
def test_unlink_should_delete_file(helper, file_id):
    data = random_str()
    offset = random_int()

    assert helper.write(file_id, data, offset) == len(data)
    helper.unlink(file_id)

    with pytest.raises(RuntimeError) as excinfo:
        helper.read(file_id, offset, len(data))
    assert 'No such file or directory' in str(excinfo.value)


@pytest.mark.links_operations_tests
def test_symlink_should_create_link(helper, mountpoint, file_id):
    dir_id = random_str()
    data = random_str()

    try:
        helper.mkdir(dir_id, 0777)
        helper.write(dir_id+"/"+file_id, data, 0)
    except:
        fail("Couldn't create directory: %s"%(dir_id))

    helper.symlink(dir_id+"/"+file_id, file_id+".lnk")

    assert helper.readlink(file_id+".lnk") == dir_id+"/"+file_id
    assert helper.read(helper.readlink(file_id+".lnk"), 0, 1024) == data


@pytest.mark.links_operations_tests
def test_link_should_create_hard_link(helper, mountpoint, file_id):
    dir_id = random_str()
    data = random_str()

    try:
        helper.mkdir(dir_id, 0777)
        helper.write(dir_id+"/"+file_id, data, 0)
    except:
        fail("Couldn't create directory: %s"%(dir_id))

    helper.link(dir_id+"/"+file_id, file_id+".lnk")

    # readlink() should only work for symlinks
    with pytest.raises(RuntimeError) as excinfo:
        helper.readlink(file_id+".lnk")
    assert 'Invalid argument' in str(excinfo.value)

    assert helper.read(file_id+".lnk", 0, 1024) == data


@pytest.mark.mknod_operations_tests
def test_mknod_should_set_premissions(helper, file_id):
    dir_id = random_str()
    data = random_str()

    flags = FlagsSet()

    helper.mknod(file_id, 0654, flags)

    assert 0777&(helper.getattr(file_id).st_mode) == 0654


@pytest.mark.mknod_operations_tests
def test_mknod_should_create_regular_file_by_default(helper, file_id):
    dir_id = random_str()
    data = random_str()

    flags = FlagsSet()

    helper.mknod(file_id, 0654, flags)

    assert Flag.IFREG in maskToFlags(helper.getattr(file_id).st_mode)
    assert not (Flag.IFCHR in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFBLK in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFIFO in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFSOCK in maskToFlags(helper.getattr(file_id).st_mode))


@pytest.mark.mknod_operations_tests
def test_mknod_should_create_regular_file_by_default(helper, file_id):
    dir_id = random_str()
    data = random_str()

    flags = FlagsSet()

    helper.mknod(file_id, 0654, flags)

    assert Flag.IFREG in maskToFlags(helper.getattr(file_id).st_mode)
    assert not (Flag.IFCHR in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFBLK in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFIFO in maskToFlags(helper.getattr(file_id).st_mode))
    assert not (Flag.IFSOCK in maskToFlags(helper.getattr(file_id).st_mode))


@pytest.mark.ownership_operations_tests
def test_chown_should_change_user_and_group(helper, file_id):
    data = random_str()

    helper.write(file_id, data, 0)

    flags = FlagsSet()

    helper.chown(file_id, 1001, 2002)
    assert helper.getattr(file_id).st_uid == 1001
    assert helper.getattr(file_id).st_gid == 2002


@pytest.mark.truncate_operations_tests
def test_truncate_should_not_create_file(helper, file_id):

    size = random_int() + 1

    with pytest.raises(RuntimeError) as excinfo:
        helper.truncate(file_id, size)
    assert 'No such file' in str(excinfo.value)
