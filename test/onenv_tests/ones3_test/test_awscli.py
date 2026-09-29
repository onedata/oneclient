"""Authors: Bartek Kryza
Copyright (C) 2026 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import hashlib
import subprocess

import pytest

from .common import random_bytes, random_path


@pytest.mark.parametrize('body', [b'', b'abcdefgh' * (1024 * 1024 // 8)], ids=['empty', '1MB'])
@pytest.mark.parametrize('checksum_algorithm', [
    'CRC64NVME', 'CRC32', 'CRC32C', 'SHA1', 'SHA256', 'SHA512',
    'XXHASH64', 'XXHASH3', 'XXHASH128',
])
def test_awscli_copy(s3_https_client, bucket_https, awscli_setup, tmp_path, body,
                     checksum_algorithm):
    key = random_path()
    source = tmp_path / 'test.txt'
    source.write_bytes(body)

    # HTTPS and an explicit algorithm select aws-chunked with a checksum
    # trailer (STREAMING-UNSIGNED-PAYLOAD-TRAILER).
    endpoint = s3_https_client.meta.endpoint_url
    assert endpoint.startswith('https://'), \
        'Streaming checksum uploads require an HTTPS endpoint'

    awscli_cmd = [
        'aws', '--no-verify-ssl',
        '--endpoint-url', endpoint,
        's3', 'cp', str(source), f's3://{bucket_https}/{key}',
        '--checksum-algorithm', checksum_algorithm,
        '--content-type', 'text/plain',
    ]

    try:
        subprocess.check_output(awscli_cmd, env=awscli_setup,
                                stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        pytest.fail(f'AWS CLI command failed: {e.output.decode(errors="replace")}')

    res = s3_https_client.get_object(Bucket=bucket_https, Key=key)
    try:
        assert res['Body'].read() == body
    finally:
        res['Body'].close()
    assert res['ContentLength'] == len(body)
    assert res['ETag'] == f'"{hashlib.md5(body).hexdigest()}"'
    assert res['Metadata'] == {}


@pytest.mark.parametrize('checksum_algorithm', ['SHA256', 'CRC32', 'CRC32C', 'CRC64NVME'])
def test_awscli_multipart_copy(s3_https_client, bucket_https, awscli_setup,
                               tmp_path, checksum_algorithm):
    part_size = 5 * 1024 * 1024
    parts = [
        b'abcdefgh' * (part_size // 8),
        b'ijklmnop' * (part_size // 8),
        b'final-short-part!',
    ]
    body = b''.join(parts)
    key = random_path()
    source = tmp_path / 'awscli-multipart-upload.bin'
    source.write_bytes(body)

    # Force three parts independently of the user's AWS CLI configuration.
    config = tmp_path / 'aws-config'
    config.write_text(
        '[default]\n'
        's3 =\n'
        '    preferred_transfer_client = classic\n'
        f'    multipart_threshold = {part_size}\n'
        f'    multipart_chunksize = {part_size}\n'
    )
    env = awscli_setup.copy()
    env['AWS_CONFIG_FILE'] = str(config)
    env['AWS_PROFILE'] = 'default'
    env['AWS_DEFAULT_PROFILE'] = 'default'

    endpoint = s3_https_client.meta.endpoint_url
    assert endpoint.startswith('https://'), \
        'Streaming checksum uploads require an HTTPS endpoint'

    awscli_cmd = [
        'aws', '--debug', '--no-verify-ssl', '--endpoint-url', endpoint,
        's3', 'cp', str(source), f's3://{bucket_https}/{key}',
        '--checksum-algorithm', checksum_algorithm,
    ]

    try:
        subprocess.check_output(awscli_cmd, env=env, stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        pytest.fail(f'AWS CLI command failed: {e.output.decode(errors="replace")}')

    part_digests = b''.join(hashlib.md5(part).digest() for part in parts)
    etag = f'"{hashlib.md5(part_digests).hexdigest()}-{len(parts)}"'
    res = s3_https_client.get_object(Bucket=bucket_https, Key=key)
    try:
        assert res['Body'].read() == body
    finally:
        res['Body'].close()
    assert res['ContentLength'] == len(body)
    assert res['ETag'] == etag
    assert res['Metadata'] == {}


def test_awscli_putobject(s3_https_client, bucket_https, awscli_setup, tmp_path):
    # Exceed the default 1 MiB aws-chunked chunk size, with a partial last chunk.
    body = random_bytes(2 * 1024 * 1024 + 17)
    key = random_path()
    source = tmp_path / 'awscli-streaming-upload.bin'
    source.write_bytes(body)

    # Trailing checksums require HTTPS; HTTP falls back to a checksum header.
    endpoint = s3_https_client.meta.endpoint_url
    assert endpoint.startswith('https://'), \
        'Streaming checksum uploads require an HTTPS endpoint'

    awscli_cmd = [
        'aws', '--no-verify-ssl', '--endpoint-url', endpoint,
        's3api', 'put-object', '--bucket', bucket_https, '--key', key,
        '--body', str(source), '--checksum-algorithm', 'CRC64NVME',
    ]

    try:
        subprocess.check_output(awscli_cmd, env=awscli_setup,
                                stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        pytest.fail(f'AWS CLI command failed: {e.output.decode(errors="replace")}')

    res = s3_https_client.get_object(Bucket=bucket_https, Key=key)
    try:
        assert res['Body'].read() == body
    finally:
        res['Body'].close()
    assert res['ContentLength'] == len(body)
    assert res['ETag'] == f'"{hashlib.md5(body).hexdigest()}"'
    assert res['Metadata'] == {}
