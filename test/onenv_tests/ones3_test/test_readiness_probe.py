"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import pytest
import botocore
import hashlib
import time
import requests
import json
from .common import *

@run_first
def test_readiness_probe(s3_client, bucket, s3_endpoint):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)

    probe_url = f"{s3_endpoint}/.__onedata__status__"
    response = requests.get(probe_url)

    assert response.status_code == 200
    try:
        json_data = response.json()
    except json.JSONDecodeError:
        assert False, "Response is not valid JSON"

    assert json_data["isOk"]
    assert "clients" not in json_data






