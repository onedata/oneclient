"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""
import os
import random
import string
import time
import requests
import boto3

from botocore.exceptions import ClientError


# Custom decorator to mark tests to run first
def run_first(test_func):
    setattr(test_func, "_run_first", True)
    return test_func


def random_int(lower_bound=1, upper_bound=100):
    return random.randint(lower_bound, upper_bound)


def random_str(size=random_int(),
               characters=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(characters) for _ in range(size))


def random_path(size=random_int(3, 10)):
    return '/'.join(random_str(5) for _ in range(size))


def random_bytes(size=random_int()):
    return random_str(size).encode('utf-8')


def get_space_id(onezone_ip, token, space_name):
    url = f'https://{onezone_ip}/api/v3/onezone/' \
          f'user/spaces'
    space_ids = requests.get(url,
                             headers={'X-Auth-Token': token,
                                      'Content-type': 'application/json'},
                             verify=False).json()["spaces"]

    for space_id in space_ids:
        url2 = f'https://{onezone_ip}/api/v3/onezone/' \
               f'user/effective_spaces/{space_id}'

        res = requests.get(url2,
                           headers={'X-Auth-Token': token,
                                    'Content-type': 'application/json'},
                           verify=False).json()

        name = res["name"]

        if name == space_name:
            return space_id

    return None


def add_user_to_space(onezone_ip, user_id, space_id, privileges):
    spaces_endpoint = f'https://{onezone_ip}/api/v3/onezone/' \
                      f'spaces/{space_id}/users/{user_id}'
    res = requests.put(spaces_endpoint, json={"privileges": privileges},
                       auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                       headers={'content-type': 'application/json'},
                       verify=False)
    assert res.ok


def remove_user_from_space(onezone_ip, user_id, space_id):
    spaces_endpoint = f'https://{onezone_ip}/api/v3/onezone/' \
                      f'spaces/{space_id}/users/{user_id}'
    res = requests.delete(spaces_endpoint,
                          auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                          headers={'content-type': 'application/json'},
                          verify=False)
    assert res.ok


def rename_space(onezone_ip, token, space_id, new_name):
    spaces_endpoint = f'https://{onezone_ip}/api/v3/onezone/' \
                      f'spaces/{space_id}'
    res = requests.patch(spaces_endpoint, json={"name": new_name},
                       #auth=requests.auth.HTTPBasicAuth('admin', 'password'),
                       headers={'X-Auth-Token': token, 'content-type': 'application/json'},
                       verify=False)
    if not res.ok:
        print(res.content)

    assert res.ok


def put_file(oneprovider_host, token, bucket_name, path, data,
             content_type='application/octet-stream'):
    url = (f'https://{oneprovider_host}/api/v3/oneprovider/'
           f'lookup-file-id/{bucket_name}')

    repeats = 5
    space_id = None
    while (repeats > 0) and (space_id is None):
        try:
            r = requests.post(url,
                              headers={'X-Auth-Token': token,
                                       'Content-type': 'application/json'},
                              verify=False)
            if r.status_code != 200:
                repeats -= 1
                continue

            space_id = r.json()["fileId"]
        except Exception as e:
            time.sleep(5)
            repeats -= 1

    url = f'https://{oneprovider_host}/api/v3/oneprovider/' \
          f'data/{space_id}/path/{path}'

    return requests.put(url,
                        data=data,
                        headers={'X-Auth-Token': token,
                                 'Content-type': content_type},
                        verify=False)


def create_presigned_url(s3_client, bucket, key, s3_method, http_method=None,
                         expiration=3600):
    """Generate a presigned URL to share an S3 object

    :param bucket_name: string
    :param key: string
    :param expiration: Time in seconds for the presigned URL to remain valid
    :return: Presigned URL as string. If error, returns None.
    """

    return s3_client.generate_presigned_url(ClientMethod=s3_method,
                                                Params={'Bucket': bucket,
                                                        'Key': key},
                                                ExpiresIn=expiration,
                                            HttpMethod=http_method)
