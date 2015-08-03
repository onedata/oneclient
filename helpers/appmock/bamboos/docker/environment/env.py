# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
"""

import os
import copy
import subprocess
import json

from . import appmock, client, common, globalregistry, provider_ccm, \
    provider_worker, docker, dns as dns_mod


def default(key):
    return {'image': 'onedata/worker',
            'bin_am': '{0}/appmock'.format(os.getcwd()),
            'bin_gr': '{0}/globalregistry'.format(os.getcwd()),
            'bin_op_worker': '{0}/op_worker'.format(os.getcwd()),
            'bin_op_ccm': '{0}/op_ccm'.format(os.getcwd()),
            'bin_oc': '{0}/oneclient'.format(os.getcwd()),
            'logdir': None}[key]


def up(config_path, image=default('image'), bin_am=default('bin_am'),
       bin_gr=default('bin_gr'), bin_op_ccm=default('bin_op_ccm'),
       bin_op_worker=default('bin_op_worker'), bin_oc=default('bin_oc'),
       logdir=default('logdir')):
    config = common.parse_json_file(config_path)
    uid = common.generate_uid()

    output = {
        'docker_ids': [],
        'gr_nodes': [],
        'gr_db_nodes': [],
        'op_ccm_nodes': [],
        'op_worker_nodes': [],
        'appmock_nodes': [],
        'client_nodes': []
    }

    # Start DNS
    [dns], dns_output = dns_mod.set_up_dns('auto', uid)
    common.merge(output, dns_output)

    # Start appmock instances
    if 'appmock' in config:
        am_output = appmock.up(image, bin_am, dns, uid, config_path)
        common.merge(output, am_output)

    # Start globalregistry instances
    if 'globalregistry' in config:
        gr_output = globalregistry.up(image, bin_gr, logdir, dns,
                                      uid, config_path)
        common.merge(output, gr_output)

    # Start provider cluster instances
    if 'providers' in config:
        # Start op_ccm instances
        op_ccm_output = provider_ccm.up(image, bin_op_ccm, logdir, dns, uid,
                                        config_path)
        common.merge(output, op_ccm_output)

        # Start op_worker instances
        op_worker_output = provider_worker.up(image, bin_op_worker, logdir, dns,
                                              uid, config_path)
        common.merge(output, op_worker_output)

    # Start oneclient instances
    if 'oneclient' in config:
        oc_output = client.up(image, bin_oc, dns, uid, config_path)
        common.merge(output, oc_output)

    # Setup global environment - providers, users, groups, spaces etc.
    if 'globalregistry' in config and 'providers' in config and 'global_setup' in config:
        providers_map = {}
        for provider_name in config['providers']:
            nodes = config['providers'][provider_name]['op_worker']['nodes'].keys()
            providers_map[provider_name] = {
                'nodes': []
            }
            for node in nodes:
                for worker_node in output['op_worker_nodes']:
                    if worker_node.startswith(node):
                        providers_map[provider_name]['nodes'].append(worker_node)
                        providers_map[provider_name]['cookie'] = \
                            config['providers'][provider_name]['op_worker']['nodes'][node]['vm.args']['setcookie']

        env_configurator_input = copy.deepcopy(config['global_setup'])
        env_configurator_input['providers'] = providers_map
        gr_node_name = config['globalregistry']['nodes'].keys()[0]
        env_configurator_input['gr_cookie'] = config['globalregistry']['nodes'][gr_node_name]['vm.args']['setcookie']
        env_configurator_input['gr_node'] = output['gr_nodes'][0]
        env_configurator_dir = '{0}/../../env_configurator'.format(common.get_script_dir())

        # Newline for clearer output
        print('')
        # Run env configurator with gathered args
        command = '''epmd -daemon
        ./env_configurator.escript \'{0}\''''
        command = command.format(json.dumps(env_configurator_input))
        docker.run(
            image='onedata/builder',
            interactive=True,
            tty=True,
            rm=True,
            workdir='/root/build',
            name=common.format_dockername('env_configurator', uid),
            volumes=[(env_configurator_dir, '/root/build', 'ro')],
            dns_list=[dns],
            command=command
        )

    return output
