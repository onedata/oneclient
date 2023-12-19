"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import pytest
import os
import sys

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, script_dir)

from .common import random_bytes, random_str, random_int, timer

import onedatafs


def test_onedatafs_should_connect_to_provider(odfs_proxy):
    space_name = 'test_onedatafs'
    test_file = random_str(10)

    handle = odfs_proxy.open(f'{space_name}/{test_file}')
    handle.close()
    odfs_proxy.unlink(f'{space_name}/{test_file}')
    odfs_proxy.close()


def test_onedatafs_should_raise_exception_on_bad_token(oneprovider_ip,
                                                       onezone_admin_token):

    with pytest.raises(RuntimeError) as excinfo:
            odfs = onedatafs.OnedataFS(
                oneprovider_ip,
                'BAD_TOKEN',
                port=443,
                insecure=True,
                force_proxy_io=True)

    assert "macaroons: macaroon invalid" in str(excinfo.value)


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
