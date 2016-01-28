# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a S3 storage.
"""

from boto.s3.connection import S3Connection, OrdinaryCallingFormat

from . import docker


def _node_up(image, buckets):
    container = docker.run(
        image=image,
        detach=True)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']
    port = 4569
    host_name = '{0}:{1}'.format(ip, port)
    access_key = 'AccessKey'
    secret_key = 'SecretKey'

    for bucket in buckets:
        connection = S3Connection(access_key, secret_key,
                                  host=ip, port=port, is_secure=False,
                                  calling_format=OrdinaryCallingFormat())
        connection.create_bucket(bucket)

    return {
        'docker_ids': [container],
        'host_name': host_name,
        'access_key': access_key,
        'secret_key': secret_key,
    }


def up(image, buckets):
    return _node_up(image, buckets)
