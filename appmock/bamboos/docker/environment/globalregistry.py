# coding=utf-8
"""Authors: Tomasz Lichoń, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of Global Registry nodes along with databases.
They can create separate clusters.
"""

import copy
import json
import os
import re

from . import common, docker, dns

LOGFILE = '/tmp/run.log'

def gr_domain(gr_instance, uid):
    """Formats domain for a GR."""
    return common.format_hostname(gr_instance, uid)


def gr_hostname(node_name, gr_instance, uid):
    """Formats hostname for a docker hosting GR.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname([node_name, gr_instance], uid)


def gr_db_hostname(db_node_name, gr_instance, uid):
    """Formats hostname for a docker hosting bigcouch for GR.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname([db_node_name, gr_instance], uid)


def db_erl_node_name(db_node_name, gr_instance, uid):
    """Formats erlang node name for a vm on GR DB docker.
    """
    hostname = gr_db_hostname(db_node_name, gr_instance, uid)
    return common.format_erl_node_name('bigcouch', hostname)


def gr_erl_node_name(node_name, gr_instance, uid):
    """Formats erlang node name for a vm on GR docker.
    """
    hostname = gr_hostname(node_name, gr_instance, uid)
    return common.format_erl_node_name('gr', hostname)


def _tweak_config(config, gr_node, gr_instance, uid):
    cfg = copy.deepcopy(config)
    cfg['nodes'] = {'node': cfg['nodes'][gr_node]}

    sys_config = cfg['nodes']['node']['sys.config']['globalregistry']
    sys_config['db_nodes'] = [db_erl_node_name(n, gr_instance, uid)
                              for n in sys_config['db_nodes']]

    if 'http_domain' in sys_config:
        sys_config['http_domain'] = {'string': gr_domain(gr_instance, uid)}

    if 'vm.args' not in cfg['nodes']['node']:
        cfg['nodes']['node']['vm.args'] = {}

    vm_args = cfg['nodes']['node']['vm.args']
    vm_args['name'] = gr_erl_node_name(gr_node, gr_instance, uid)

    return cfg


def _node_up(gr_id, domain, gr_ips, dns_ips, dns_config, gen_dev_config):
    """Updates dns.config and starts the GR node"""
    ip_addresses = {
        domain: gr_ips
    }
    ns_servers = []
    for i in range(len(dns_ips)):
        ns = 'ns{0}.{1}'.format(i, domain)
        ns_servers.append(ns)
        ip_addresses[ns] = [dns_ips[i]]
    primary_ns = ns_servers[0]
    mail_exchange = 'mail.{0}'.format(domain)
    ip_addresses[mail_exchange] = [gr_ips[0]]
    admin_mailbox = 'dns-admin.{0}'.format(domain)

    cname = '{{cname, "{0}"}},'.format(domain)
    dns_config = re.sub(
        re.compile(r"\{cname,\s*[^\}]*\},", re.MULTILINE),
        cname,
        dns_config)

    ip_addresses_entries = []
    for address in ip_addresses:
        ip_list = '"{0}"'.format('","'.join(ip_addresses[address]))
        ip_addresses_entries.append('        {{"{0}", [{1}]}}'
                                    .format(address, ip_list))
    ip_addresses = '{{ip_addresses, [\n{0}\n    ]}},'.format(
        ',\n'.join(ip_addresses_entries))
    dns_config = re.sub(
        re.compile(r"\{ip_addresses,\s*\[(\s*\{[^\}]*\}[,]?\s*)*\]\},",
                   re.MULTILINE),
        ip_addresses,
        dns_config)

    ns_servers = '{{ns_servers, [\n        "{0}"\n    ]}},'.format(
        '",\n        "'.join(ns_servers))
    dns_config = re.sub(
        re.compile(r"\{ns_servers,\s*\[[^\]\}]*\]\},", re.MULTILINE),
        ns_servers,
        dns_config)

    mail_exchange = '{{mail_exchange, [\n        {{10, "{0}"}}\n    ]}},' \
        .format(mail_exchange)
    dns_config = re.sub(
        re.compile(r"\{mail_exchange,\s*\[[^\]]*\]\},", re.MULTILINE),
        mail_exchange,
        dns_config)

    primary_ns = '{{primary_ns, "{0}"}},'.format(primary_ns)
    dns_config = re.sub(
        re.compile(r"\{primary_ns,\s*[^\}]*\},", re.MULTILINE),
        primary_ns,
        dns_config)

    admin_mailbox = '{{admin_mailbox, "{0}"}},'.format(admin_mailbox)
    dns_config = re.sub(
        re.compile(r"\{admin_mailbox,\s*[^\}]*\},", re.MULTILINE),
        admin_mailbox,
        dns_config)

    gr_command = '''set -e
mkdir -p /root/bin/node/log/
echo 'while ((1)); do chown -R {uid}:{gid} /root/bin/node/log; sleep 1; done' > /root/bin/chown_logs.sh
bash /root/bin/chown_logs.sh &
cat <<"EOF" > /tmp/gen_dev_args.json
{gen_dev_args}
EOF
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
cat <<"EOF" > /root/bin/node/resources/dns.config
{dns_config}
EOF
/root/bin/node/bin/globalregistry console >> {logfile}'''
    gr_command = gr_command.format(
        uid=os.geteuid(),
        gid=os.getegid(),
        gen_dev_args=json.dumps({'globalregistry': gen_dev_config}),
        dns_config=dns_config,
        logfile=LOGFILE)

    docker.exec_(
        container=gr_id,
        detach=True,
        interactive=True,
        tty=True,
        command=gr_command)


def _docker_up(image, bindir, config, dns_servers, logdir):
    """Starts the docker but does not start GR
    as dns.config update is needed first
    """
    node_name = config['nodes']['node']['vm.args']['name']
    cookie = config['nodes']['node']['vm.args']['setcookie']
    db_nodes = config['nodes']['node']['sys.config']['globalregistry']['db_nodes']

    (gr_name, sep, gr_hostname) = node_name.partition('@')

    # Start DB node for current GR instance.
    # Currently, only one DB node for GR is allowed, because we are using links.
    # It's impossible to create a bigcouch cluster with docker's links.
    db_node = db_nodes[0]
    (db_name, sep, db_hostname) = db_node.partition('@')

    db_command = '''echo '[httpd]' > /opt/bigcouch/etc/local.ini
echo 'bind_address = 0.0.0.0' >> /opt/bigcouch/etc/local.ini
sed -i 's/-name bigcouch/-name {name}@{host}/g' /opt/bigcouch/etc/vm.args
sed -i 's/-setcookie monster/-setcookie {cookie}/g' /opt/bigcouch/etc/vm.args
/opt/bigcouch/bin/bigcouch'''
    db_command = db_command.format(name=db_name, host=db_hostname,
                                   cookie=cookie)

    bigcouch = docker.run(
        image='onedata/bigcouch',
        name=db_hostname,
        hostname=db_hostname,
        detach=True,
        command=db_command)

    volumes = [(bindir, '/root/build', 'ro')]

    if logdir:
        logdir = os.path.join(os.path.abspath(logdir), gr_hostname)
        volumes.extend([(logdir, '/root/bin/node/log', 'rw')])

    # Just start the docker, GR will be started later when dns.config is updated
    gr = docker.run(
        image=image,
        name=gr_hostname,
        hostname=gr_hostname,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/build',
        volumes=volumes,
        dns_list=dns_servers,
        link={db_hostname: db_hostname},
        command=['bash'])

    return gr, {
        'docker_ids': [bigcouch, gr],
        'gr_db_nodes': ['{0}@{1}'.format(db_name, db_hostname)],
        'gr_nodes': ['{0}@{1}'.format(gr_name, gr_hostname)]
    }


def up(image, bindir, dns_server, uid, config_path, logdir=None):
    config = common.parse_json_config_file(config_path)
    input_dir = config['dirs_config']['globalregistry']['input_dir']
    dns_servers, output = dns.maybe_start(dns_server, uid)

    for gr_instance in config['globalregistry_domains']:
        gen_dev_cfg = {
            'config': {
                'input_dir': input_dir,
                'target_dir': '/root/bin'
            },
            'nodes': config['globalregistry_domains'][gr_instance]['globalregistry']
        }

        tweaked_configs = [_tweak_config(gen_dev_cfg, gr_node, gr_instance, uid)
                           for gr_node in gen_dev_cfg['nodes']]

        gr_ips = []
        gr_configs = {}
        for cfg in tweaked_configs:
            gr, node_out = _docker_up(image, bindir, cfg, dns_servers, logdir)
            common.merge(output, node_out)
            gr_configs[gr] = cfg
            gr_ips.append(common.get_docker_ip(gr))

        domain = gr_domain(gr_instance, uid)

        dns_cfg_path = os.path.join(os.path.abspath(bindir),
                                    input_dir, 'resources', 'dns.config')
        orig_dns_cfg = open(dns_cfg_path).read()
        # Update dns.config file on each GR node
        for id in gr_configs:
            _node_up(id, domain, gr_ips, gr_ips, orig_dns_cfg, gr_configs[id])

        domains = {
            'domains': {
                domain: {
                    'ns': gr_ips,
                    'a': []
                }
            }
        }
        common.merge(output, domains)

    # Make sure domains are added to the dns server
    dns.maybe_restart_with_configuration(dns_server, uid, output)

    return output
