"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import pprint
import hashlib
import random

import pytest
from .common import random_str


def random_range(r):
    res = list(range(r))
    random.shuffle(res)
    return res

@pytest.mark.parametrize(
    "parts,order",
    [
        pytest.param(
            list([b'1' * 1_000]), range(1)
        ),
        pytest.param(
            list([b'1' * 1_000, b'1' * 1_000, b'1' * 1_000, b'1' * 1_000]), range(4)
        ),
        pytest.param(
            list([b'1' * 6_000_000, b'2' * 6_000_000, b'3' * 1_000_000]), range(3)
        ),
        pytest.param(
            [b'1' * 6_000_000, b'2' * 6_000_000, b'3' * 1_000_000], reversed(list(range(3)))
        ),
        # TODO
        #pytest.param(
        #    list([b'1' * (6_000_000+i*1024) for i in range(5)]), random_range(5)
        #),
    ],
)
def test_multipart_upload(s3_client, bucket, parts, order):
    key = random_str()

    parts_md5 = [hashlib.md5(p).digest() for p in parts]
    parts_etags = [hashlib.md5(p).hexdigest() for p in parts]
    complete_file = b''.join(parts)

    pprint.pprint(parts_etags)

    multipart_md5 = b''.join(parts_md5)

    multipart_etag = hashlib.md5(multipart_md5).hexdigest() + "-" + str(len(parts))

    res = s3_client.create_multipart_upload(Bucket=bucket, Key=key, ContentType='image/jpeg')

    pprint.pprint(res)

    assert (res['Key'] == key)
    upload_id = res['UploadId']

    for i in order:
        part_number = i + 1
        res = s3_client.upload_part(Bucket=bucket, Key=key, Body=parts[i], PartNumber=part_number, UploadId=upload_id)
        print(res)
        assert (res['ETag'] == '"' + parts_etags[i] + '"')

    res = s3_client.list_parts(Bucket=bucket, Key=key, UploadId=upload_id)

    pprint.pprint(res)

    assert (len(res['Parts']) == len(parts))
    for p in res['Parts']:
        assert (p['ETag'] == '"' + parts_etags[p["PartNumber"] - 1] + '"')



    res = s3_client.complete_multipart_upload(Bucket=bucket, Key=key, UploadId=upload_id,
                                              MultipartUpload={
                                                  'Parts': [{'ETag': p['ETag'], 'PartNumber': p['PartNumber']} for p in
                                                            res['Parts']]})

    pprint.pprint(res)

    assert (res['Key'] == key)
    assert (res['ETag'] == f'"{multipart_etag}"')

    res = s3_client.head_object(Bucket=bucket, Key=key)

    pprint.pprint(res)

    assert (res['ContentType'] == 'image/jpeg')
    assert(res['ETag'] == f'"{multipart_etag}"')

    res = s3_client.get_object(Bucket=bucket, Key=key)
    assert(res['ContentLength'] == len(complete_file))
    assert(res['Body'].read() == complete_file)
