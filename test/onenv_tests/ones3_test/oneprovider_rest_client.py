"""Authors: Bartek Kryza
Copyright (C) 2023 onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'
"""

import requests

def get_space_id(oneprovider_host, token, bucket_name):
    url = f'https://{oneprovider_host}/api/v3/oneprovider/lookup-file-id/{bucket_name}'
    return requests.post(url,
                        headers={'X-Auth-Token': token,
                                 'Content-type': 'application/json'},
                        verify=False).json()["fileId"]


def put_file(oneprovider_host, token, bucket_name, path, data,
             content_type = 'application/octet-stream'):

    space_id = get_space_id(oneprovider_host, token, bucket_name)

    url = f'https://{oneprovider_host}/api/v3/oneprovider/data/{space_id}/path/{path}'

    return requests.put(url,
                     data=data,
                     headers={'X-Auth-Token': token,
                              'Content-type': content_type},
                     verify=False)