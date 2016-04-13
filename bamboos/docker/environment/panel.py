# coding=utf-8
"""Author: Krzysztof Trzepla
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of onepanel nodes. They can create separate clusters.
"""

import copy
import json
import os

from . import common, docker, dns


def panel_hostname(node_name, uid):
    """Formats hostname for a docker hosting onepanel.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname(node_name, uid)


def panel_erl_node_name(node_name, uid):
    """Formats erlang node name for a vm on onepanel docker.
    """
    hostname = panel_hostname(node_name, uid)
    return common.format_erl_node_name('onepanel', hostname)


def panel_application_domain(uid):
    """Formats application domain for a docker hosting onepanel."""
    return common.format_hostname([], uid)


def _tweak_config(config, name, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][name]}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = panel_erl_node_name(name, uid)

    sys_config = cfg['nodes']['node']['sys.config']['onepanel']
    sys_config['application_domain'] = {"string": panel_application_domain(uid)}

    return cfg


def _node_up(image, bindir, config, oz_ip, dns_servers, packages,
             storages):
    node_name = config['nodes']['node']['vm.args']['name']

    (name, sep, hostname) = node_name.partition('@')

    command = \
        '''set -e
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
/root/bin/node/bin/op_panel console'''
    command = command.format(
        gen_dev_args=json.dumps({'onepanel': config}),
        uid=os.geteuid(),
        gid=os.getegid())

    volumes = [(bindir, '/root/build', 'ro')]

    for name in config['nodes']['node']['storages']:
        volumes.append((storages[name]['host'], storages[name]['container'],
                        'rw'))

    container = docker.run(
        image=image,
        name=hostname,
        hostname=hostname,
        add_host={'onedata.org': oz_ip},
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        volumes=volumes,
        dns_list=dns_servers,
        run_params=["--privileged"],
        command=command)

    for name in config['nodes']['node']['packages']:
        package = os.path.join('/root/build', packages[name])
        docker.exec_(container, ['dpkg', '-i', package])
        docker.exec_(container, ['apt-get', '-f', '-y', 'install'])

    return (
        {
            'docker_ids': [container],
            'onepanel_nodes': [node_name]
        }
    )


def up(image, bindir, dns_server, uid, config_path, oz_ip):
    config = common.parse_json_config_file(config_path)
    input_dir = config['dirs_config']['onepanel']['input_dir']
    packages = config['onepanel']['packages']
    storages = config['onepanel']['storages']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    gen_dev_cfg = {
        'config': {
            'input_dir': input_dir,
            'target_dir': '/root/bin'
        },
        'nodes': config['onepanel']['nodes']
    }

    configs = [_tweak_config(gen_dev_cfg, node, uid)
               for node in gen_dev_cfg['nodes']]

    for cfg in configs:
        node_out = _node_up(image, bindir, cfg, oz_ip, dns_servers, packages,
                            storages)
        common.merge(output, node_out)

    return output
