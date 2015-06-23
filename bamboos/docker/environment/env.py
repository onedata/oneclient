# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
"""

import os

from . import appmock, client, common, globalregistry, provider_ccm, \
    provider_worker, dns as dns_mod


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

    # Start op_ccm instances
    if 'op_ccm' in config:
        op_ccm_output = provider_ccm.up(image, bin_op_ccm, logdir, dns, uid,
                                        config_path)
        common.merge(output, op_ccm_output)

    # Start op_worker instances
    if 'op_worker' in config:
        op_worker_output = provider_worker.up(image, bin_op_worker, logdir, dns,
                                              uid, config_path)
        common.merge(output, op_worker_output)

    # Start oneclient instances
    if 'oneclient' in config:
        oc_output = client.up(image, bin_oc, dns, uid, config_path)
        common.merge(output, oc_output)

    return output
