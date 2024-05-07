"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib
import pytest
import time
from concurrent.futures import ThreadPoolExecutor, wait, ALL_COMPLETED

import requests

from .common import *
from .big_list_of_naughty_strings import big_list_of_naughty_strings


def test_put_object_simple(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_get_object_readonly_token(s3_client, s3_readonly_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_readonly_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_naughty_file_names(s3_client, bucket, thread_count=25):
    def put_get_object(job):
        bucket_, key_, body_ = job
        etag_ = hashlib.md5(body_).hexdigest()
        s3_client.put_object(Bucket=bucket_, Key=key_, Body=body_)
        res = s3_client.get_object(Bucket=bucket_, Key=key_)
        assert (res['ContentLength'] == len(body_))
        assert (res['ETag'] == f'"{etag_}"')

    with ThreadPoolExecutor(thread_count) as pool:
        jobs = [(bucket, f'{random_str()}/{name}', random_bytes(4)) \
                for name in big_list_of_naughty_strings]
        futures = [pool.submit(put_get_object, job) for job in jobs]
        wait(futures, timeout=30, return_when=ALL_COMPLETED)


def test_put_object_1B(s3_client, bucket):
    key = random_path()

    body = b'x'
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_2B(s3_client, bucket):
    key = random_path()

    body = b'xy'
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_1MB(s3_client, bucket):
    key = random_path()

    body = b'x' * 1024 * 1024
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_4MB(s3_client, bucket):
    key = random_path()

    body = b'x' * 1024 * 1024 * 4
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_10MB(s3_client, bucket):
    key = random_path()

    body = b'x' * 1024 * 1024 * 10
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)


def test_put_object_overwrite(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')


@pytest.mark.parametrize(
    "prefix",
    [
        pytest.param('dir1/'), pytest.param('dir1/dir2/dir3/dir4/')
    ],
)
def test_put_object_with_prefix(s3_client, bucket, prefix):
    key = prefix + random_str()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')


@pytest.mark.parametrize(
    "prefix_list,count",
    [
        pytest.param(
            ['dir1/', 'dir1/dir2/', 'dir1/dir2/', 'dir2/dir3/dir4/dir5/'], 10),
        pytest.param(['dir50/'], 50)
    ],
)
def test_put_and_get_objects(s3_client, bucket, prefix_list, count):
    keys = {}

    for prefix in prefix_list:
        for i in range(count):
            body = random_bytes()
            etag = hashlib.md5(body).hexdigest()
            key = f'{prefix}file-{i}.txt'

            s3_client.put_object(Bucket=bucket, Key=key, Body=body)

            keys[key] = {'body': body, 'etag': etag}

    for key in keys.keys():
        res = s3_client.get_object(Bucket=bucket, Key=key)
        assert (res['ContentLength'] == len(keys[key]['body']))
        assert (res['ETag'] == f'"{keys[key]["etag"]}"')


@pytest.mark.parametrize(
    "prefix_list,count",
    [
        pytest.param(
            ['dir1/', 'dir1/dir2/', 'dir1/dir2/', 'dir2/dir3/dir4/dir5/'], 250),
        pytest.param(['dir50/'], 1250)
    ],
)
def test_put_objects_large(bucket, s3_client, prefix_list, count,
                           thread_count=100):
    keys = []

    def put_object(job):
        bucket_, key_, body_, etag_ = job
        res = s3_client.put_object(Bucket=bucket_, Key=key_, Body=body_)
        assert (res['ContentLength'] == len(body_))
        assert (res['ETag'] == f'"{etag_}"')

    for prefix in prefix_list:
        for i in range(count):
            body = random_bytes()
            etag = hashlib.md5(body).hexdigest()
            key = f'{prefix}file-{i}.txt'
            keys.append((bucket, key, body, etag))

    with ThreadPoolExecutor(thread_count) as pool:
        jobs = keys
        futures = [pool.submit(put_object, job) for job in jobs]
        wait(futures, timeout=30, return_when=ALL_COMPLETED)


def test_put_same_object_in_path_multiple(s3_client, uuid_str, bucket):
    name = f'dir1/dir2/dir3/dir4/dir5/dir6/dir7/dir8/dir9/{uuid_str}'
    thread_count = 25
    file_count = 100

    def task(args):
        key = args[0]

        body = random_bytes(10)
        etag = hashlib.md5(body).hexdigest()

        s3_client.put_object(Bucket=bucket, Key=key, Body=body)

    executor = ThreadPoolExecutor(thread_count)
    futs = []
    for i in range(0, file_count):
        futs.append(executor.submit(task, (name,)))

    wait(futs)

    failed = 0

    for f in futs:
        if f.exception():
            print(f.exception())
            failed += 1

    assert (failed == 0)


def test_delete_object(s3_client, bucket):
    key = random_path()

    body = random_bytes()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)

    s3_client.get_object(Bucket=bucket, Key=key)

    s3_client.delete_object(Bucket=bucket, Key=key)

    with pytest.raises(s3_client.exceptions.ClientError) as excinfo:
        s3_client.get_object(Bucket=bucket, Key=key)

    assert 'Not Found' in str(excinfo.value)


def test_delete_objects(s3_client, bucket):
    body = random_bytes()

    files = [f'f-{i}.txt' for i in range(10)]

    for f in files:
        s3_client.put_object(Bucket=bucket, Key=f, Body=body)

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter='/',
                                    EncodingType='path', MaxKeys=1000,
                                    Prefix='')

    assert (res['KeyCount'] == 10)

    s3_client.delete_objects(Bucket=bucket,
                             Delete={'Objects': [{'Key': f} for f in files]})

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter='/',
                                    EncodingType='path', MaxKeys=1000,
                                    Prefix='')

    assert (res['KeyCount'] == 0)


def test_get_object_range(s3_client, bucket):
    key = random_path()

    body = random_bytes(10)
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.get_object(Bucket=bucket, Key=key, Range="bytes=2-4")

    assert (res['ContentLength'] == 3)
    assert (res['ContentRange'] == 'bytes 2-4/10')
    assert (res['ResponseMetadata']['HTTPStatusCode'] == 206)
    assert (res['ResponseMetadata']['HTTPHeaders']['content-range']
            == 'bytes 2-4/10')
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body[2:5])


@pytest.mark.parametrize(
    "size",
    [
        pytest.param(1024), pytest.param(5 * 1024 * 1024)
    ],
)
def test_get_object_remote(s3_client, oneprovider_2_ip, onezone_admin_token,
                           size):
    class FileLocationNotYetReplicated(Exception):
        "Raised when file location is not yet replicated between providers"
        pass

    bucket = 'test_get_object_remote'
    key = random_str()

    data = random_bytes(size)

    retries = 5
    success = False
    while retries > 0 and not success:
        try:
            r = put_file(oneprovider_2_ip, onezone_admin_token, bucket, key,
                         data)

            if r.status_code != 201:
                raise FileLocationNotYetReplicated

            success = True
        except Exception as e:
            # Wait for the file to show up at oneprovider 1
            time.sleep(2)
        finally:
            retries = retries - 1

    retries = 10
    success = False
    while retries > 0 and not success:
        try:
            res = s3_client.get_object(Bucket=bucket, Key=key)

            if res['ContentLength'] < len(data):
                raise FileLocationNotYetReplicated

            assert (res['ContentLength'] == len(data))
            assert (res['Body'].read() == data)

            success = True
        except (
        s3_client.exceptions.ClientError, FileLocationNotYetReplicated) as e:
            # Wait for the file to show up at oneprovider 1
            time.sleep(2)
        finally:
            retries = retries - 1

    assert (success)


@pytest.mark.parametrize(
    "size",
    [
        pytest.param(1024), pytest.param(5 * 1024 * 1024)
    ],
)
def test_get_object_remote_readonly_token(s3_client, s3_readonly_client,
                                          oneprovider_2_ip, onezone_admin_token,
                                          size):
    bucket = 'test_get_object_remote'
    key = random_str()

    data = random_bytes(size)

    r = put_file(oneprovider_2_ip, onezone_admin_token, bucket, key, data)

    assert (r.status_code == 201)

    retries = 10
    while retries > 0:
        try:
            res = s3_readonly_client.get_object(Bucket=bucket, Key=key)

            assert (res['ContentLength'] == len(data))
            assert (res['Body'].read() == data)

            break
        except s3_client.exceptions.ClientError as e:
            # Wait for the file to show up at oneprovider 1
            time.sleep(2)
        finally:
            retries = retries - 1

    assert (retries > 0)


def test_get_object_range_multiple(s3_client, bucket, uuid_str):
    name = uuid_str
    thread_count = 100
    file_count = 500

    def task(args):
        key = f'dir0/dir1/dir2/dir3/dir4/dir5/dir6/dir7/dir8/dir9/{random_str()}'

        body = random_bytes(10)
        etag = hashlib.md5(body).hexdigest()

        s3_client.put_object(Bucket=bucket, Key=key, Body=body)
        res = s3_client.get_object(Bucket=bucket, Key=key, Range="bytes=2-4")

        assert (res['ContentLength'] == 3)
        assert (res['ContentRange'] == 'bytes 2-4/10')
        assert (res['ResponseMetadata']['HTTPStatusCode'] == 206)
        assert (res['ResponseMetadata']['HTTPHeaders']['content-range']
                == 'bytes 2-4/10')
        assert (res['ETag'] == f'"{etag}"')
        assert (res['Body'].read() == body[2:5])

        s3_client.delete_object(Bucket=bucket, Key=key)

    executor = ThreadPoolExecutor(thread_count)
    futs = []
    for i in range(0, file_count):
        futs.append(executor.submit(task, (name,)))

    wait(futs)

    failed = 0

    for f in futs:
        if f.exception():
            print(f.exception())
            failed += 1

    assert (failed == 0)


def test_head_object(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.head_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')


def test_head_object_readonly_token(s3_client, s3_readonly_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_readonly_client.head_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')


def test_set_content_type(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body,
                         ContentType='application/pdf')
    res = s3_client.head_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ContentType'] == 'application/pdf')
    assert (res['ETag'] == f'"{etag}"')


def test_autodetect_content_type(s3_client, bucket):
    key = random_path() + ".png"

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.head_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ContentType'] == 'image/png')
    assert (res['ETag'] == f'"{etag}"')


def test_autodetect_unknown_content_type(s3_client, bucket):
    key = random_path() + "unknownextension"

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body)
    res = s3_client.head_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ContentType'] == 'application/octet-stream')
    assert (res['ETag'] == f'"{etag}"')


@pytest.mark.skip
def test_copy_object(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body,
                         ContentType='text/plain')

    res = s3_client.copy_object(Bucket=bucket, CopySource=bucket + "/" + key,
                                Key=f'{key}-copy')

    res = s3_client.get_object(Bucket=bucket, Key=f'{key}-copy')

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['ContentType'] == 'text/plain')


def test_get_object_presigned(s3_client, bucket):
    key = random_path()

    body = random_bytes()
    etag = hashlib.md5(body).hexdigest()

    s3_client.put_object(Bucket=bucket, Key=key, Body=body,
                         ContentType='text/plain')

    presigned_url = create_presigned_url(s3_client, bucket, key, 'get_object',
                                         'GET')

    response = requests.get(presigned_url, data=body)

    assert (response.status_code == 200)
    assert (response.headers['ETag'] == f'"{etag}"')
    assert (response.headers['Content-Length'] == str(len(body)))
    assert (response.headers['Access-Control-Allow-Origin'] == '*')
    assert (response.content == body)


@pytest.mark.parametrize(
    "file_size",
    [
        pytest.param(20),
        pytest.param(25*1024*1024)
    ],
)
def test_put_object_presigned(s3_client, bucket, file_size):
    key = random_path()

    body = random_bytes(file_size)
    etag = hashlib.md5(body).hexdigest()

    presigned_url = create_presigned_url(s3_client, bucket, key, 'put_object',
                                         'PUT')

    response = requests.put(presigned_url, data=body)

    assert (response.status_code == 200)
    assert (response.headers['ETag'] == f'"{etag}"')

    res = s3_client.get_object(Bucket=bucket, Key=key)

    assert (res['ContentLength'] == len(body))
    assert (res['ETag'] == f'"{etag}"')
    assert (res['Body'].read() == body)
