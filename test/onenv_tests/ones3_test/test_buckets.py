"""Authors: Bartek Kryza
Copyright (C) 2021 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import pytest
import botocore
import hashlib
import time
from concurrent.futures import ThreadPoolExecutor, wait, ALL_COMPLETED
from .common import *
from .conftest import s3_client


def test_create_delete_bucket(s3_client, uuid_str, s3_server):
    name = uuid_str

    s3_client.create_bucket(Bucket=name, CreateBucketConfiguration={
        'LocationConstraint': 'pl-reg-w3'})
    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == name, buckets)).count(True) == 1)

    s3_client.delete_bucket(Bucket=name)

    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == name, buckets)).count(True) == 0)


def test_create_delete_nonempty_bucket(s3_client, uuid_str):
    name = uuid_str

    s3_client.create_bucket(Bucket=name, CreateBucketConfiguration={
        'LocationConstraint': 'pl-reg-w3'})
    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == name, buckets)).count(True) == 1)

    s3_client.put_object(Bucket=name, Key='file.txt', Body=b'TEST')

    with pytest.raises(Exception,
                       match="The bucket you tried to delete is not empty") as excinfo:
        s3_client.delete_bucket(Bucket=name)

    assert 'The bucket you tried to delete is not empty' in str(excinfo.value)

    s3_client.delete_object(Bucket=name, Key='file.txt')

    res = s3_client.list_objects_v2(Bucket=name, Delimiter='/',
                                    EncodingType='url', MaxKeys=1000, Prefix='')

    assert (res['KeyCount'] == 0)

    s3_client.delete_bucket(Bucket=name)

    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == name, buckets)).count(True) == 0)


def test_create_buckets_with_the_same_name_and_write(s3_client, uuid_str):
    name = uuid_str
    thread_count = 10
    bucket_count = 50

    def task(args):
        s3_client2 = s3_client
        bucket_name = args[0]
        res = None
        try:
            res = s3_client2.create_bucket(
                Bucket=bucket_name,
                CreateBucketConfiguration={'LocationConstraint': 'pl-reg-k1'})
        except botocore.exceptions.ClientError as e:
            if e.response['Error']['Code'] == 'BucketAlreadyOwnedByYou':
                pass
            else:
                raise e
        except Exception as e:
            raise Exception(f'Bucket creation failed for bucket '
                            f'{bucket_name} due to {e}')

        body = b'xyz'
        key = random_str()
        try:
            s3_client2.put_object(Bucket=bucket_name, Key=key, Body=body)
        except Exception as e:
            raise e

        s3_client2.delete_object(Bucket=bucket_name, Key=key)

        if res:
            return f'Bucket {bucket_name} created successfully and ' \
                   f'test file created'
        else:
            return f'Test file created'

    executor = ThreadPoolExecutor(thread_count)
    futs = []
    for i in range(0, bucket_count):
        futs.append(executor.submit(task, (name,)))

    wait(futs)

    failed = 0

    for f in futs:
        if f.exception():
            print(f.exception())
            failed += 1
        else:
            print(f.result())

    print("Done...")
    if failed > 0:
        print(f'{failed} out of {bucket_count} failed to create')
    else:
        print('All buckets created successfully')

    assert (failed == 0)

    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert list(map(lambda b: b['Name'] == name, buckets)).count(True) == 1, \
        "There should be only 1 bucket"

    s3_client.delete_bucket(Bucket=name)

    res = s3_client.list_buckets()
    buckets = res['Buckets']

    assert list(map(lambda b: b['Name'] == name, buckets)).count(True) == 0, \
        "There should be no buckets after its deleted"


def test_list_buckets_by_another_user(s3_client_joe, bucket,
                                      onezone_admin_token,
                                      onezone_ip,
                                      user_joe_id):
    space_id = get_space_id(onezone_ip, onezone_admin_token, bucket)
    add_user_to_space(onezone_ip, user_joe_id, space_id, ['space_view'])

    # Wait until infer token scope can see that the user was added to the space
    time.sleep(10)

    res = s3_client_joe.list_buckets()
    buckets = res['Buckets']

    remove_user_from_space(onezone_ip, user_joe_id, space_id)

    assert list(map(lambda b: b['Name'] == bucket, buckets)).count(True) == 1


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_empty_bucket(s3_client, bucket, encoding_type, delimiter):
    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert ('Contents' not in res)
    assert (res['Name'] == bucket)


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_v2_empty_bucket(s3_client, bucket, encoding_type, delimiter):
    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    assert (res['KeyCount'] == 0)
    assert (res['Name'] == bucket)


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_small_bucket(s3_client, bucket, encoding_type, delimiter):
    for i in range(20):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i}.txt', Body=b'TEST')

    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert (len(res['Contents']) == 20)
    assert (res['Name'] == bucket)


def test_list_small_bucket_by_another_user(s3_client, s3_client_joe, bucket,
                                           onezone_admin_token,
                                           onezone_ip,
                                           user_joe_id):
    for i in range(20):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i}.txt', Body=b'TEST')

    space_id = get_space_id(onezone_ip, onezone_admin_token, bucket)
    add_user_to_space(onezone_ip, user_joe_id, space_id,
                      ['space_view', 'space_read_data'])

    # Wait until infer token scope can see that the user was added to the space
    time.sleep(10)

    res = s3_client_joe.list_objects(Bucket=bucket, Delimiter='/',
                                     EncodingType='path', MaxKeys=1000,
                                     Prefix='')

    assert (len(res['Contents']) == 20)
    assert (res['Name'] == bucket)

    remove_user_from_space(onezone_ip, user_joe_id, space_id)


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_small_bucket_and_verify_etag(s3_client, bucket, encoding_type,
                                           delimiter):
    file_count = 20
    files_content = []
    files_md5 = []
    for i in range(file_count):
        body = random_bytes()
        etag = hashlib.md5(body).hexdigest()
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt', Body=body)
        files_content.append(body)
        files_md5.append(etag)

    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert (len(files_md5) == file_count)
    assert (len(files_content) == file_count)
    assert (len(res['Contents']) == file_count)
    assert (res['Name'] == bucket)

    for i in range(len(res['Contents'])):
        assert (res['Contents'][i]['ETag'] == f'"{files_md5[i]}"')


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_small_bucket_and_verify_etag_readonly_token(s3_client,
                                                          s3_readonly_client,
                                                          bucket, encoding_type,
                                                          delimiter):
    file_count = 20
    files_content = []
    files_md5 = []
    for i in range(file_count):
        body = random_bytes()
        etag = hashlib.md5(body).hexdigest()
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt', Body=body)
        files_content.append(body)
        files_md5.append(etag)

    res = s3_readonly_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                          EncodingType=encoding_type,
                                          MaxKeys=1000,
                                          Prefix='')

    assert (len(files_md5) == file_count)
    assert (len(files_content) == file_count)
    assert (len(res['Contents']) == file_count)
    assert (res['Name'] == bucket)

    for i in range(len(res['Contents'])):
        assert (res['Contents'][i]['ETag'] == f'"{files_md5[i]}"')


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_single_file(s3_client, bucket, encoding_type, delimiter):
    s3_client.put_object(Bucket=bucket, Key=f'dir1/dir2/file1.txt',
                         Body=b'TEST')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='dir1/dir2/file1.txt')

    assert (len(res['Contents']) == 1)
    assert (res['KeyCount'] == 1)
    assert (res['Name'] == bucket)
    assert (res['Contents'][0]['Key'] == 'dir1/dir2/file1.txt')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='dir1/dir2')

    assert (len(res['Contents']) == 1)
    assert (res['KeyCount'] == 1)
    assert (res['Name'] == bucket)
    assert (res['Contents'][0]['Key'] == 'dir1/dir2/file1.txt')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='dir1/dir2/')

    assert (len(res['Contents']) == 1)
    assert (res['KeyCount'] == 1)
    assert (res['Name'] == bucket)
    assert (res['Contents'][0]['Key'] == 'dir1/dir2/file1.txt')


@pytest.mark.skip
@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_single_file_as_folder(s3_client, bucket, encoding_type,
                                    delimiter):
    s3_client.put_object(Bucket=bucket, Key=f'dir1/dir2/file1.txt',
                         Body=b'TEST')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='dir1/dir2/file1.txt/')

    assert (res['KeyCount'] == 0)
    assert (res['Name'] == bucket)


@pytest.mark.parametrize(
    "encoding_type,delimiter",
    [
        pytest.param('path', '/'), pytest.param('url', '/'),
        pytest.param('path', ''), pytest.param('url', '')
    ],
)
def test_list_v2_small_bucket(s3_client, bucket, encoding_type, delimiter):
    for i in range(20):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i}.txt', Body=b'TEST')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    assert (res['KeyCount'] == 20)
    assert (res['Name'] == bucket)
    assert (res['Name'] == bucket)


@pytest.mark.parametrize(
    "encoding_type,size,step,delimiter",
    [
        pytest.param('path', 11, 2, '/'), pytest.param('url', 11, 2, '/'),
        pytest.param('path', 11, 2, ''), pytest.param('url', 11, 2, '')
    ]
)
def test_list_big_bucket(s3_client, bucket, encoding_type, size, step,
                         delimiter):
    for i in range(size):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i}.txt', Body=b'TEST')

    contents = []
    next_marker = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                     EncodingType=encoding_type, MaxKeys=step,
                                     Prefix='', Marker=next_marker)

        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['Marker'] == next_marker)
        if has_more:
            next_marker = res['NextMarker']

    assert (len(contents) == size)


@pytest.mark.parametrize(
    "encoding_type,size,step,delimiter",
    [
        pytest.param('path', 11, 2, '/'), pytest.param('url', 11, 2, '/'),
        pytest.param('path', 11, 2, ''), pytest.param('url', 11, 2, ''),
        pytest.param('path', 1042, 1000, '')
    ]
)
def test_list_v2_big_bucket(s3_client, bucket, encoding_type, size, step,
                            delimiter, thread_count=25):
    def put_object(job):
        bucket_, key_, body_, etag_ = job
        res = s3_client.put_object(Bucket=bucket_, Key=key_, Body=body_)
        assert (res['ContentLength'] == len(body_))
        assert (res['ETag'] == f'"{etag_}"')

    jobs = []
    for i in range(size):
        body = b'TEST'
        etag = hashlib.md5(body).hexdigest()
        key = f'file-{i}.txt'
        jobs.append((bucket, key, body, etag))

    with ThreadPoolExecutor(thread_count) as pool:
        futures = [pool.submit(put_object, job) for job in jobs]
        wait(futures, timeout=30, return_when=ALL_COMPLETED)

    contents = []
    continuation_token = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                        EncodingType=encoding_type,
                                        MaxKeys=step,
                                        Prefix='',
                                        ContinuationToken=continuation_token)
        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['ContinuationToken'] == continuation_token)
        if has_more:
            continuation_token = res['NextContinuationToken']

    assert (len(contents) == size)


def test_list_objects_recursive_skips_hidden_mpu_directory(s3_client, bucket):
    delimiter = ''
    encoding_type = 'path'
    key = random_str()

    parts = list([random_bytes() for i in range(50)])

    parts_md5 = [hashlib.md5(p).digest() for p in parts]
    parts_etags = [hashlib.md5(p).hexdigest() for p in parts]

    multipart_md5 = b''.join(parts_md5)

    res = s3_client.create_multipart_upload(Bucket=bucket, Key=key,
                                            ContentType='image/jpeg')

    assert (res['Key'] == key)
    upload_id = res['UploadId']

    for i in range(len(parts)):
        part_number = i + 1
        res = s3_client.upload_part(Bucket=bucket, Key=key, Body=parts[i],
                                    PartNumber=part_number, UploadId=upload_id)
        assert (res['ETag'] == '"' + parts_etags[i] + '"')

    # Now list objects with delimiter and ensure that there are
    # no '.__s3__mpus__' prefixes
    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    assert ('Contents' not in res)

    s3_client.put_object(Bucket=bucket, Key='.__s3__mpus__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.__hidden__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.hidden/file.txt', Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='file.txt', Body=b'TEST')

    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert ('Contents' in res)
    assert (len(res['Contents']) == 3)

    for o in res['Contents']:
        assert (not o['Key'].startswith('.__s3__mpus__'))

    size = 20
    step = 3
    for i in range(size):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt',
                             Body=b'TEST')

    contents = []
    next_marker = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                     EncodingType=encoding_type, MaxKeys=step,
                                     Prefix='', Marker=next_marker)

        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['Marker'] == next_marker)
        if has_more:
            next_marker = res['NextMarker']

    assert (len(contents) == size + 3)


def test_list_objects_v2_recursive_skips_hidden_mpu_directory(s3_client,
                                                              bucket):
    delimiter = ''
    encoding_type = 'path'
    key = random_str()

    parts = list([random_bytes() for i in range(50)])

    parts_md5 = [hashlib.md5(p).digest() for p in parts]
    parts_etags = [hashlib.md5(p).hexdigest() for p in parts]

    multipart_md5 = b''.join(parts_md5)

    res = s3_client.create_multipart_upload(Bucket=bucket, Key=key,
                                            ContentType='image/jpeg')

    assert (res['Key'] == key)
    upload_id = res['UploadId']

    for i in range(len(parts)):
        part_number = i + 1
        res = s3_client.upload_part(Bucket=bucket, Key=key, Body=parts[i],
                                    PartNumber=part_number, UploadId=upload_id)
        assert (res['ETag'] == '"' + parts_etags[i] + '"')

    # Now list objects with delimiter and ensure that there are
    # no '.__s3__mpus__' prefixes
    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    assert ('Contents' not in res)

    s3_client.put_object(Bucket=bucket, Key='.__s3__mpus__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.__hidden__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.hidden/file.txt', Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='file.txt', Body=b'TEST')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    if delimiter == '':
        assert ('Contents' in res)
        assert (len(res['Contents']) == 3)

        for o in res['Contents']:
            assert (not o['Key'].startswith('.__s3__mpus__'))
    else:
        assert ('Contents' in res)
        assert (len(res['Contents']) == 1)

        for o in res['CommonPrefixes']:
            assert (not o['Prefix'].startswith('.__s3__mpus__'))

    size = 20
    step = 3
    for i in range(size):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt',
                             Body=b'TEST')

    contents = []
    continuation_token = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                        EncodingType=encoding_type,
                                        MaxKeys=step,
                                        Prefix='',
                                        ContinuationToken=continuation_token)

        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['ContinuationToken'] == continuation_token)
        if has_more:
            continuation_token = res['NextContinuationToken']

    assert (len(contents) == size + 3)


def test_list_objects_skips_hidden_mpu_directory(s3_client, bucket):
    delimiter = '/'
    encoding_type = 'path'
    key = random_str()

    parts = list([random_bytes() for i in range(50)])

    parts_md5 = [hashlib.md5(p).digest() for p in parts]
    parts_etags = [hashlib.md5(p).hexdigest() for p in parts]

    multipart_md5 = b''.join(parts_md5)

    res = s3_client.create_multipart_upload(Bucket=bucket, Key=key,
                                            ContentType='image/jpeg')

    assert (res['Key'] == key)
    upload_id = res['UploadId']

    for i in range(len(parts)):
        part_number = i + 1
        res = s3_client.upload_part(Bucket=bucket, Key=key, Body=parts[i],
                                    PartNumber=part_number, UploadId=upload_id)
        assert (res['ETag'] == '"' + parts_etags[i] + '"')

    # Now list objects with delimiter and ensure that there are
    # no '.__s3__mpus__' prefixes
    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert ('Contents' not in res)

    s3_client.put_object(Bucket=bucket, Key='.__s3__mpus__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.__hidden__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.hidden/file.txt', Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='file.txt', Body=b'TEST')

    res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                 EncodingType=encoding_type, MaxKeys=1000,
                                 Prefix='')

    assert ('Contents' in res)
    assert (len(res['Contents']) == 1)

    for o in res['CommonPrefixes']:
        assert (not o['Prefix'].startswith('.__s3__mpus__'))

    size = 20
    step = 3
    for i in range(size):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt',
                             Body=b'TEST')

    contents = []
    next_marker = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects(Bucket=bucket, Delimiter=delimiter,
                                     EncodingType=encoding_type, MaxKeys=step,
                                     Prefix='', Marker=next_marker)

        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['Marker'] == next_marker)
        if has_more:
            next_marker = res['NextMarker']

    assert (len(contents) == size + 1)


def test_list_objects_v2_skips_hidden_mpu_directory(s3_client, bucket):
    delimiter = '/'
    encoding_type = 'path'
    key = random_str()

    parts = list([random_bytes() for i in range(50)])

    parts_md5 = [hashlib.md5(p).digest() for p in parts]
    parts_etags = [hashlib.md5(p).hexdigest() for p in parts]

    multipart_md5 = b''.join(parts_md5)

    res = s3_client.create_multipart_upload(Bucket=bucket, Key=key,
                                            ContentType='image/jpeg')

    assert (res['Key'] == key)
    upload_id = res['UploadId']

    for i in range(len(parts)):
        part_number = i + 1
        res = s3_client.upload_part(Bucket=bucket, Key=key, Body=parts[i],
                                    PartNumber=part_number, UploadId=upload_id)
        assert (res['ETag'] == '"' + parts_etags[i] + '"')

    # Now list objects with delimiter and ensure that there are
    # no '.__s3__mpus__' prefixes
    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    assert ('Contents' not in res)

    s3_client.put_object(Bucket=bucket, Key='.__s3__mpus__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.__hidden__/file.txt',
                         Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='.hidden/file.txt', Body=b'TEST')
    s3_client.put_object(Bucket=bucket, Key='file.txt', Body=b'TEST')

    res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                    EncodingType=encoding_type, MaxKeys=1000,
                                    Prefix='')

    if delimiter == '':
        assert ('Contents' in res)
        assert (len(res['Contents']) == 3)

        for o in res['Contents']:
            assert (not o['Key'].startswith('.__s3__mpus__'))
    else:
        assert ('Contents' in res)
        assert (len(res['Contents']) == 1)

        for o in res['CommonPrefixes']:
            assert (not o['Prefix'].startswith('.__s3__mpus__'))

    size = 20
    step = 3
    for i in range(size):
        s3_client.put_object(Bucket=bucket, Key=f'file-{i:0>4}.txt',
                             Body=b'TEST')

    contents = []
    continuation_token = ""
    has_more = True
    while has_more:
        res = s3_client.list_objects_v2(Bucket=bucket, Delimiter=delimiter,
                                        EncodingType=encoding_type,
                                        MaxKeys=step,
                                        Prefix='',
                                        ContinuationToken=continuation_token)

        if 'Contents' in res:
            contents.extend(res['Contents'])
        has_more = res['IsTruncated']
        assert (res['Name'] == bucket)
        assert (res['ContinuationToken'] == continuation_token)
        if has_more:
            continuation_token = res['NextContinuationToken']

    assert (len(contents) == size + 1)


def test_bucket_spaceid_cache_invalidates_entries(s3_client_bucket_cache_invalidation,
                                                    onezone_admin_token,
                                                    onezone_ip,
                                                    uuid_str):
    bucket = uuid_str
    bucket_new = uuid_str+"_NEW"
    key = 'file.txt'

    s3_client_bucket_cache_invalidation.create_bucket(Bucket=bucket, CreateBucketConfiguration={
        'LocationConstraint': 'pl-reg-w3'})
    res = s3_client_bucket_cache_invalidation.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == bucket, buckets)).count(True) == 1)

    s3_client_bucket_cache_invalidation.put_object(Bucket=bucket, Key=key, Body=b'TEST')

    space_id = get_space_id(onezone_ip, onezone_admin_token, bucket)

    rename_space(onezone_ip, onezone_admin_token, space_id, bucket_new)

    # The bucket should still work here under the old name
    res = s3_client_bucket_cache_invalidation.get_object(Bucket=bucket, Key=key)
    assert (res['Body'].read() == b'TEST')

    time.sleep(4)

    # Now the old bucket name should not be visible
    with pytest.raises(s3_client_bucket_cache_invalidation.exceptions.ClientError) as excinfo:
        s3_client_bucket_cache_invalidation.get_object(Bucket=bucket, Key=key)

    assert 'NoSuchBucket' in str(excinfo.value)

    # But the bucket should work under new name
    res = s3_client_bucket_cache_invalidation.get_object(Bucket=bucket_new, Key=key)
    assert (res['Body'].read() == b'TEST')

    res = s3_client_bucket_cache_invalidation.list_buckets()
    buckets = res['Buckets']

    assert (list(map(lambda b: b['Name'] == bucket_new, buckets)).count(True) == 1)

    s3_client_bucket_cache_invalidation.delete_object(Bucket=bucket_new, Key=key)

    s3_client_bucket_cache_invalidation.delete_bucket(Bucket=bucket_new)
