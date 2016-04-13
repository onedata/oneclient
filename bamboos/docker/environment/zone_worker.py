"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of cluster-worker nodes. They can create separate clusters.
"""

import os
import re
from . import worker, common, gui_livereload

DOCKER_BINDIR_PATH = '/root/build'


def up(image, bindir, dns_server, uid, config_path, logdir=None,
       dnsconfig_path=None):
    if dnsconfig_path is None:
        config = common.parse_json_config_file(config_path)
        input_dir = config['dirs_config']['oz_worker']['input_dir']

        dnsconfig_path = os.path.join(os.path.abspath(bindir), input_dir,
                                      'data', 'dns.config')

    return worker.up(image, bindir, dns_server, uid, config_path,
                     OZWorkerConfigurator(dnsconfig_path), logdir)


class OZWorkerConfigurator:
    def __init__(self, dnsconfig_path):
        self.dnsconfig_path = dnsconfig_path

    def tweak_config(self, cfg, uid, instance):
        sys_config = cfg['nodes']['node']['sys.config'][self.app_name()]
        if 'http_domain' in sys_config:
            domain = worker.cluster_domain(instance, uid)
            sys_config['http_domain'] = {'string': domain}
        # If livereload bases on gui output dir mount, change the location
        # from where static files are served to that dir.
        if 'gui_livereload' in cfg:
            if cfg['gui_livereload'] in ['mount_output', 'mount_output_poll']:
                sys_config['gui_custom_static_root'] = {
                    'string': '/root/gui_static'}
        return cfg

    def configure_started_instance(self, bindir, instance, config,
                                   container_ids, output, storages_dockers=None):
        this_config = config[self.domains_attribute()][instance]
        # Check if gui_livereload is enabled in env and turn it on
        if 'gui_livereload' in this_config:
            mode = this_config['gui_livereload']
            if mode != 'none':
                print '''\
Starting GUI livereload
    zone: {0}
    mode: {1}'''.format(instance, mode)
                for container_id in container_ids:
                    gui_livereload.run(
                        container_id,
                        os.path.join(bindir, 'rel/gui.config'),
                        'rel/oz_worker',
                        DOCKER_BINDIR_PATH,
                        '/root/bin/node',
                        mode=mode)

    def pre_start_commands(self, bindir, config, domain, worker_ips):
        dnsconfig_path = self.dnsconfig_path
        dns_config = open(dnsconfig_path).read()

        oz_ips = worker_ips
        ip_addresses = {
            domain: ["ALL"]
        }

        ns_servers = ['ns.{0}'.format(domain)]
        primary_ns = ns_servers[0]
        ip_addresses[primary_ns] = ["ALL"]

        mail_exchange = 'mail.{0}'.format(domain)
        ip_addresses[mail_exchange] = [oz_ips[0]]
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

        mail_exchange = '{{mail_exchange, [\n        {{10, "{0}"}}\n    ]}},'. \
            format(mail_exchange)
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

        return dns_config

    def extra_volumes(self, config, bindir):
        extra_volumes = []
        # Check if gui_livereload is enabled in env and add required volumes
        if 'gui_livereload' in config:
            extra_volumes += gui_livereload.required_volumes(
                os.path.join(bindir, 'rel/gui.config'),
                bindir,
                'rel/oz_worker',
                DOCKER_BINDIR_PATH,
                mode=config['gui_livereload'])
        return extra_volumes

    def app_name(self):
        return "oz_worker"

    def domains_attribute(self):
        return "zone_domains"

    def domain_env_name(self):
        return "zone_domain"

    def nodes_list_attribute(self):
        return "oz_worker_nodes"
