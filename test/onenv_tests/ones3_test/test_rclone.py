"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib
import pytest
import subprocess
import pprint
from .common import random_bytes, random_str, random_path


def test_rclone_copy(s3_client, bucket, rclone_setup):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    # Create a sample file to fill the bucket-space cache
    s3_client.put_object(Bucket=bucket, Key=key, Body=body)

    rclone_cmd = f'rclone --no-check-certificate --transfers 5 ' \
                 f'--no-update-modtime --retries 1 copy ./src/ s3proxy:{bucket}'

    try:
        subprocess.check_output(rclone_cmd.split(' '))
    except subprocess.CalledProcessError as e:
        assert False, "rclone command failed " + str(e.stdout)

    test_file = 'main.cc'

    res = s3_client.get_object(Bucket=bucket, Key=test_file)

    pprint.pprint(res)

    assert(res['Metadata'] == {})
