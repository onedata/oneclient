# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
"""

import os
import sys
import copy
import json
import time
from . import appmock, client, common, zone_worker, cluster_manager, \
    worker, provider_worker, cluster_worker, docker, dns, s3, ceph, nfs, \
    amazon_iam, luma


def default(key):
    return {'image': 'onedata/worker',
            'ceph_image': 'onedata/ceph',
            's3_image': 'lphoward/fake-s3',
            'nfs_image': 'erezhorev/dockerized_nfs_server',
            'bin_am': '{0}/appmock'.format(os.getcwd()),
            'bin_oz': '{0}/oz_worker'.format(os.getcwd()),
            'bin_op_worker': '{0}/op_worker'.format(os.getcwd()),
            'bin_cluster_worker': '{0}/cluster_worker'.format(os.getcwd()),
            'bin_cluster_manager': '{0}/cluster_manager'.format(os.getcwd()),
            'bin_oc': '{0}/oneclient'.format(os.getcwd()),
            'bin_luma': '{0}/luma'.format(os.getcwd()),
            'logdir': None}[key]


def up(config_path, image=default('image'), ceph_image=default('ceph_image'),
       s3_image=default('s3_image'), nfs_image=default('nfs_image'),
       bin_am=default('bin_am'), bin_oz=default('bin_oz'),
       bin_cluster_manager=default('bin_cluster_manager'),
       bin_op_worker=default('bin_op_worker'),
       bin_cluster_worker=default('bin_cluster_worker'),
       bin_oc=default('bin_oc'), bin_luma=default('bin_luma'),
       logdir=default('logdir')):
    config = common.parse_json_config_file(config_path)
    uid = common.generate_uid()

    output = {
        'docker_ids': [],
        'oz_worker_nodes': [],
        'oz_db_nodes': [],
        'cluster_manager_nodes': [],
        'op_worker_nodes': [],
        'cluster_worker_nodes': [],
        'appmock_nodes': [],
        'client_nodes': []
    }

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(output, dns_output)

    # Start appmock instances
    if 'appmock_domains' in config:
        am_output = appmock.up(image, bin_am, dns_server, uid, config_path, logdir)
        common.merge(output, am_output)
        # Make sure appmock domains are added to the dns server.
        # Setting first arg to 'auto' will force the restart and this is needed
        # so that dockers that start after can immediately see the domains.
        dns.maybe_restart_with_configuration('auto', uid, output)

    # Start provider cluster instances
    setup_worker(zone_worker, bin_oz, 'zone_domains',
                 bin_cluster_manager, config, config_path, dns_server, image,
                 logdir, output, uid)

    # Start storages
    storages_dockers, storages_dockers_ids = \
        _start_storages(config, config_path, ceph_image, s3_image, nfs_image,
                        image, uid)
    output['storages'] = storages_dockers

    # Start python LUMA service
    luma_config = None
    if 'provider_domains' in config:
        luma_config = _start_luma(config, storages_dockers, image, bin_luma,
                                  output, uid)

    # Start provider cluster instances
    setup_worker(provider_worker, bin_op_worker, 'provider_domains',
                 bin_cluster_manager, config, config_path, dns_server, image,
                 logdir, output, uid, storages_dockers, luma_config)

    # Start stock cluster worker instances
    setup_worker(cluster_worker, bin_cluster_worker, 'cluster_domains',
                 bin_cluster_manager, config, config_path, dns_server, image,
                 logdir, output, uid)

    # Start oneclient instances
    if 'oneclient' in config:
        oc_output = client.up(image, bin_oc, dns_server, uid, config_path,
                              logdir, storages_dockers)
        common.merge(output, oc_output)

    # Add storages at the end so they will be deleted after other dockers
    output['docker_ids'].extend(storages_dockers_ids)

    # Setup global environment - providers, users, groups, spaces etc.
    if 'zone_domains' in config and \
            'provider_domains' in config and \
            'global_setup' in config:
        providers_map = {}
        for provider_name in config['provider_domains']:
            providers_map[provider_name] = {
                'nodes': [],
                'cookie': ''
            }
            for cfg_node in config['provider_domains'][provider_name][
                'op_worker'].keys():
                providers_map[provider_name]['nodes'].append(
                    worker.worker_erl_node_name(cfg_node, provider_name, uid))
                providers_map[provider_name]['cookie'] = \
                    config['provider_domains'][provider_name]['op_worker'][
                        cfg_node]['vm.args']['setcookie']

        env_configurator_input = copy.deepcopy(config['global_setup'])
        env_configurator_input['provider_domains'] = providers_map

        # For now, take only the first node of the first OZ
        # as multiple OZs are not supported yet.
        env_configurator_input['oz_cookie'] = \
            config['zone_domains'].values()[0][
                'oz_worker'].values()[0]['vm.args']['setcookie']
        env_configurator_input['oz_node'] = output['oz_worker_nodes'][0]

        env_configurator_dir = '{0}/../../env_configurator'.format(
            common.get_script_dir())

        # Newline for clearer output
        print('')
        # Run env configurator with gathered args
        command = '''epmd -daemon
./env_configurator.escript \'{0}\'
echo $?'''
        command = command.format(json.dumps(env_configurator_input))
        docker_output = docker.run(
            image='onedata/builder',
            interactive=True,
            tty=True,
            rm=True,
            workdir='/root/build',
            name=common.format_hostname('env_configurator', uid),
            volumes=[(env_configurator_dir, '/root/build', 'ro')],
            dns_list=[dns_server],
            command=command,
            output=True
        )
        # Result will contain output from env_configurator and result code in
        # the last line
        lines = docker_output.split('\n')
        command_res_code = lines[-1]
        command_output = '\n'.join(lines[:-1])
        # print the output
        print(command_output)
        # check of env configuration succeeded
        if command_res_code != '0':
            # Let the command_output be flushed to console
            time.sleep(5)
            sys.exit(1)

    return output


def setup_worker(worker, bin_worker, domains_name, bin_cm, config, config_path,
                 dns_server, image, logdir, output, uid, storages_dockers=None,
                 luma_config=None):
    if domains_name in config:
        # Start cluster_manager instances
        cluster_manager_output = cluster_manager.up(image, bin_cm, dns_server,
                                                    uid, config_path, logdir,
                                                    domains_name=domains_name)
        common.merge(output, cluster_manager_output)

        # Start op_worker instances
        cluster_worker_output = worker.up(image, bin_worker, dns_server, uid,
                                          config_path, logdir,
                                          storages_dockers=storages_dockers,
                                          luma_config=luma_config)
        common.merge(output, cluster_worker_output)
        # Make sure OP domains are added to the dns server.
        # Setting first arg to 'auto' will force the restart and this is needed
        # so that dockers that start after can immediately see the domains.
        dns.maybe_restart_with_configuration('auto', uid, output)


def _start_storages(config, config_path, ceph_image, s3_image, nfs_image, image,
                    uid):
    storages_dockers = {'ceph': {}, 's3': {}, 'nfs': {}, 'posix': {}}
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
                    pool = tuple(storage['pool'].split(':'))
                    result = ceph.up(ceph_image, [pool], storage['name'], uid)
                    docker_ids.extend(result['docker_ids'])
                    del result['docker_ids']
                    storages_dockers['ceph'][storage['name']] = result
                elif storage['type'] == 's3' and storage['name'] not in \
                        storages_dockers['s3']:
                    result = s3.up(s3_image, [storage['bucket']],
                                   storage['name'], uid)
                    docker_ids.extend(result['docker_ids'])
                    del result['docker_ids']

                    start_iam_mock = _want_start_iam_mock(storage)
                    if 'iam_host' in storage and 'iam_request_scheme' in storage:
                        result['iam_host'] = storage['iam_host']
                        result['iam_request_scheme'] = storage['iam_request_scheme']

                    storages_dockers['s3'][storage['name']] = result
                elif storage['type'] == 'nfs' and storage['name'] not in \
                        storages_dockers['nfs']:
                    result = nfs.up(nfs_image, uid, storage['name'])
                    docker_ids.extend(result['docker_ids'])

                    # create system users and groups on nfs docker
                    container = result['docker_ids'][0]
                    common.create_users(container, cfg['users'])
                    common.create_groups(container, cfg['groups'])

                    del result['docker_ids']
                    storages_dockers['nfs'][storage['name']] = result

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


def _start_luma(config, storages_dockers, image, bin_luma, output, uid):
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
