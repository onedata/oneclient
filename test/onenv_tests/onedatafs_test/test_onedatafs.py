"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import pytest
import os
import sys
import time

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, script_dir)

from .common import random_bytes, random_str, random_int, timer, put_file

import onedatafs


def test_onedatafs_should_connect_to_provider(odfs_proxy):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    handle = odfs_proxy.open(f'{space_name}/{test_file}')
    handle.close()
    odfs_proxy.unlink(f'{space_name}/{test_file}')
    odfs_proxy.close()


def test_onedatafs_create_destroy_instance(onezone_ip, onezone_admin_token):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    for i in range(0,10):
        odfs = onedatafs.OnedataFS(
            onezone_ip,
            onezone_admin_token,
            port=443,
            insecure=True,
            force_proxy_io=True)
        odfs.close()


def test_onedatafs_should_raise_exception_on_bad_token(onezone_ip,
                                                       onezone_admin_token):

    with pytest.raises(RuntimeError) as excinfo:
            odfs = onedatafs.OnedataFS(
                onezone_ip,
                'BAD_TOKEN',
                port=443,
                insecure=True,
                force_proxy_io=True)
            odfs.readdir('', 1000, 0)

    assert "HTTP Exception" in str(excinfo.value)


def test_onedatafs_read_write(odfs_proxy):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    handle = odfs_proxy.open(f'{space_name}/{test_file}')
    handle.write(b'TEST', 0)
    chunk = handle.read(0, 4)
    handle.close()

    assert(chunk == b'TEST')

    odfs_proxy.unlink(f'{space_name}/{test_file}')
    odfs_proxy.close()


def test_onedatafs_read_write_direct(odfs_direct):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    handle = odfs_direct.open(f'{space_name}/{test_file}')
    handle.write(b'TEST', 0)
    chunk = handle.read(0, 4)
    handle.close()

    assert(chunk == b'TEST')

    odfs_direct.unlink(f'{space_name}/{test_file}')
    odfs_direct.close()


def test_onedatafs_stat(odfs_proxy):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    handle = odfs_proxy.open(f'{space_name}/{test_file}')
    handle.write(b'TEST', 0)
    chunk = handle.read(0, 4)
    handle.close()
    assert(chunk == b'TEST')

    attrs = odfs_proxy.stat(f'{space_name}/{test_file}')
    assert attrs.size == 4
    assert attrs.mode == 0o100644

    odfs_proxy.unlink(f'{space_name}/{test_file}')
    odfs_proxy.close()


def test_onedatafs_dir_handling(odfs_proxy):
    space_name = 'test_onedatafs'
    test_dir = random_str(10)

    dir = f'{space_name}/{test_dir}'

    attrs = odfs_proxy.mkdir(dir)

    attrs = odfs_proxy.stat(dir)
    assert attrs.mode == 0o40755

    dir_handle = odfs_proxy.opendir(dir)
    odfs_proxy.releasedir(dir, dir_handle)

    odfs_proxy.unlink(dir)
    odfs_proxy.close()


def test_onedatafs_readdir(odfs_proxy):
    space_name = 'test_onedatafs'
    test_dir = random_str(10)

    dir = f'{space_name}/{test_dir}'

    attrs = odfs_proxy.mkdir(dir)

    file_list = [f'f_{i}' for i in range(0,10)]

    for f in file_list:
        odfs_proxy.open(f'{dir}/{f}').close()

    dirs = odfs_proxy.readdir(dir, 1000, 0)

    assert set(dirs) == set(file_list)


def test_onedatafs_rename(odfs_proxy):
    space_name = 'test_onedatafs'
    test_dir = random_str(10)

    dir = f'{space_name}/{test_dir}'

    attrs = odfs_proxy.mkdir(dir)
    test_file = random_str(10)

    handle = odfs_proxy.open(f'{dir}/{test_file}')
    handle.write(b'TEST', 0)
    chunk = handle.read(0, 4)
    handle.close()
    assert(chunk == b'TEST')

    odfs_proxy.rename(f'{dir}/{test_file}',
                      f'{space_name}/{test_file}')

    with pytest.raises(RuntimeError) as excinfo:
        odfs_proxy.stat(f'{dir}/{test_file}')

    assert "No such file or directory" in str(excinfo.value)

    odfs_proxy.stat(f'{space_name}/{test_file}')

    odfs_proxy.unlink(f'{space_name}/{test_file}')
    odfs_proxy.close()


def test_onedatafs_xattr_handling(odfs_proxy):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    file = f'{space_name}/{test_file}'

    handle = odfs_proxy.open(file)
    handle.write(b'TEST', 0)
    chunk = handle.read(0, 4)
    handle.close()
    assert(chunk == b'TEST')

    odfs_proxy.setxattr(file, 'license', '\"CC-0\"', True, False)
    assert 'license' in odfs_proxy.listxattr(file)
    assert odfs_proxy.getxattr(file, 'license') == b'\"CC-0\"'
    odfs_proxy.removexattr(file, 'license')
    assert 'license' not in odfs_proxy.listxattr(file)

    odfs_proxy.unlink(file)
    odfs_proxy.close()


@pytest.mark.parametrize(
    "size",
    [
        pytest.param(1024), pytest.param(5 * 1024 * 1024)
    ],
)
def test_read_file_remote(odfs_proxy, oneprovider_ip, onezone_admin_token,
                           size):
    class FileLocationNotYetReplicated(Exception):
        "Raised when file location is not yet replicated between providers"
        pass

    space_name = 'test_get_object_remote'
    file_name = random_str()

    data = random_bytes(size)

    retries = 5
    success = False
    while retries > 0 and not success:
        try:
            r = put_file(oneprovider_ip, onezone_admin_token, space_name,
                         file_name, data)

            if r.status_code != 201:
                raise FileLocationNotYetReplicated

            success = True
        except Exception as e:
            # Wait for the file to show up at oneprovider 1
            time.sleep(2)
        finally:
            retries = retries - 1

    assert success

    retries = 10
    success = False
    while retries > 0 and not success:
        try:
            # wait for the file to appear in the provider
            attr = odfs_proxy.stat(f'{space_name}/{file_name}')
            if attr.size < size:
                raise FileLocationNotYetReplicated
            handle = odfs_proxy.open(f'{space_name}/{file_name}')
            chunk = handle.read(0, size)
            handle.close()
            if len(chunk) < size:
                raise FileLocationNotYetReplicated

            assert (chunk == data)

            success = True
        except Exception as e:
            # Wait for the file to show up at oneprovider 1
            time.sleep(2)
        finally:
            retries = retries - 1

    odfs_proxy.unlink(f'{space_name}/{file_name}')
    odfs_proxy.close()

    assert success



