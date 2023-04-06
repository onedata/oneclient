"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib
import pytest
import subprocess
import pprint
import pathlib
import os

from .common import random_bytes, random_str, random_path


def test_minio_copy(s3_client, bucket, minio_setup):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    source = str(os.path.join(pathlib.Path().resolve(), 'src'))
    minio_cmd = f'mc --insecure cp --recursive {source} s3proxy/{bucket}'

    try:
        output = subprocess.check_output('mc alias list'.split(' '))
        assert 's3proxy' in output.decode('utf-8')

        output = subprocess.check_output(minio_cmd.split(' '))
        print("--- Minio complete successfully \n" + output.decode('utf-8'))
    except subprocess.CalledProcessError as e:
        assert False, "mc command failed " + str(e.stdout)

    test_file = 'src/main.cc'

    res = s3_client.get_object(Bucket=bucket, Key=test_file)

    pprint.pprint(res)

    assert(res['Metadata'] == {})


@pytest.mark.parametrize(
    "file_size,file_count,duration",
    [
        pytest.param('1MiB', 50, '0m30s'),
        pytest.param('1KiB', 500, '0m30s')
    ],
)
def test_minio_warp_mixed(s3_client, bucket, minio_setup, s3_host, s3_port,
                          onezone_admin_token, secret_access_key, file_size,
                          file_count, duration):

    warp_cmd = f"warp --insecure get " \
               f"--concurrent 10 " \
               f"--obj.size {file_size} " \
               f"--objects {file_count} " \
               f"--host {s3_host}:{s3_port} " \
               f"--bucket {bucket} " \
               f"--duration {duration} " \
               f"--access-key {onezone_admin_token} " \
               f"--secret-key {secret_access_key}"

    try:
        output = subprocess.check_output('mc alias list'.split(' '))
        assert 's3proxy' in output.decode('utf-8')

        output = subprocess.check_output(warp_cmd.split(' '))
        print("--- Minio warp mixed completed successfully \n" +
              output.decode('utf-8'))
    except subprocess.CalledProcessError as e:
        assert False, "mc command failed " + str(e.stdout)


@pytest.mark.parametrize(
    "file_size,duration",
    [
        pytest.param('10MiB', '0m15s')
    ],
)
def test_minio_warp_multipart(s3_client, bucket, minio_setup, s3_host,
                              s3_port, onezone_admin_token, secret_access_key,
                              file_size, duration):

    warp_cmd = f"warp --insecure multipart " \
               f"--concurrent 10 --parts=25 --part.size {file_size} " \
               f"--host {s3_host}:{s3_port} " \
               f"--bucket {bucket} " \
               f"--duration {duration} " \
               f"--access-key {onezone_admin_token} " \
               f"--secret-key {secret_access_key}"

    try:
        output = subprocess.check_output('mc alias list'.split(' '))
        assert 's3proxy' in output.decode('utf-8')

        output = subprocess.check_output(warp_cmd.split(' '))
        print("--- Minio warp multipart completed successfully \n" +
              output.decode('utf-8'))
    except subprocess.CalledProcessError as e:
        assert False, "mc command failed " + str(e.stdout)