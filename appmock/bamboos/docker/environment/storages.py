# coding=utf-8
"""Authors: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Contains methods used to bring up storages.
"""
import sys

from . import common, s3, ceph, nfs, amazon_iam, luma, swift


def start_luma(config, storages_dockers, image, bin_luma, output, uid):
    enable_luma_proxy = False
    for key in config['provider_domains']:
        if config['provider_domains'][key].get('enable_luma_proxy'):
            enable_luma_proxy = True
            break
    luma_config = None
    if enable_luma_proxy:
        if 'luma_setup' not in config:
            luma_config = luma.get_default_config()
            if storages_dockers['ceph']:
                ceph_config = storages_dockers['ceph'].values()[0]
                luma_config['generators_config']['ceph']['key'] = \
                    ceph_config['key']
                luma_config['generators_config']['ceph']['mon_host'] = \
                    ceph_config['host_name']
            config['luma_setup'] = luma_config

        luma_config = luma.up(image, bin_luma, config, uid)
        output['docker_ids'].extend(luma_config['docker_ids'])
        output['luma'] = {'host_name': luma_config['host_name']}
    return luma_config


def start_storages(config, config_path, ceph_image, s3_image, nfs_image,
                    swift_image, image, uid):
    storages_dockers = {'ceph': {}, 's3': {}, 'nfs': {}, 'posix': {},
                        'swift': {}}
    docker_ids = []
    if 'os_configs' in config:
        start_iam_mock = False
        for key, cfg in config['os_configs'].iteritems():
            for storage in cfg['storages']:
                if isinstance(storage, basestring):
                    sys.stderr.write('''WARNING:
        Detected deprecated syntax at os_configs.{0}.storages
        Change entry "{1}" to: {{ "type": "posix", "name": "{1}" }}
        In file {2}'''.format(key, storage, config_path))
                    break

                if storage['type'] == 'ceph' and storage['name'] not in \
                        storages_dockers['ceph']:
                    _ceph_up(storage, storages_dockers, ceph_image, docker_ids,
                             uid)

                elif storage['type'] == 's3' and storage['name'] not in \
                        storages_dockers['s3']:
                    start_iam_mock = _want_start_iam_mock(storage)
                    _s3_up(storage, storages_dockers, s3_image, docker_ids, uid)

                elif storage['type'] == 'swift' and storage['name'] not in \
                        storages_dockers['swift']:
                    _swift_up(storage, storages_dockers, swift_image,
                              docker_ids, uid)

                elif storage['type'] == 'nfs' and storage['name'] not in \
                        storages_dockers['nfs']:
                    _nfs_up(storage, storages_dockers, nfs_image, docker_ids,
                            uid, cfg)

        if start_iam_mock:
            docker_ids.extend(_start_iam_mock(image, uid, storages_dockers))

    return storages_dockers, docker_ids


def _want_start_iam_mock(storage):
    return 'iam_host' not in storage and 'request_scheme' not in storage and \
           not storage.get('disable_iam_mock', False)


def _start_iam_mock(image, uid, storages_dockers):
    iam_mock_config = amazon_iam.up(image, uid)

    iam_request_scheme = 'http'
    iam_host = iam_mock_config['host_name']
    for key in storages_dockers['s3'].keys():
        if 'iam_host' not in storages_dockers['s3'][key] and \
                        'request_scheme' not in storages_dockers['s3'][key]:
            storages_dockers['s3'][key]['iam_host'] = iam_host
            storages_dockers['s3'][key][
                'iam_request_scheme'] = iam_request_scheme

    return iam_mock_config['docker_ids']


def _ceph_up(storage, storages_dockers, ceph_image, docker_ids, uid):
    pool = tuple(storage['pool'].split(':'))
    result = ceph.up(ceph_image, [pool], storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['ceph'][storage['name']] = result


def _s3_up(storage, storages_dockers, s3_image, docker_ids, uid):
    result = s3.up(s3_image, [storage['bucket']],
                                   storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']

    if 'iam_host' in storage and 'iam_request_scheme' in storage:
        result['iam_host'] = storage['iam_host']
        result['iam_request_scheme'] = storage[
            'iam_request_scheme']

    storages_dockers['s3'][storage['name']] = result


def _swift_up(storage, storages_dockers, swift_image, docker_ids, uid):
    result = swift.up(swift_image, [storage['container']],
                                   storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']

    storages_dockers['swift'][storage['name']] = result


def _nfs_up(storage, storages_dockers, nfs_image, docker_ids, uid, cfg):
    result = nfs.up(nfs_image, uid, storage['name'])
    docker_ids.extend(result['docker_ids'])

    # create system users and groups on nfs docker
    container = result['docker_ids'][0]
    common.create_users(container, cfg['users'])
    common.create_groups(container, cfg['groups'])

    del result['docker_ids']
    storages_dockers['nfs'][storage['name']] = result
