"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of cluster-worker nodes. They can create separate clusters.
"""

import os
from . import docker, common, worker, gui, panel

def up(image, bindir, dns_server, uid, config_path, logdir=None,
       dnsconfig_path=None, storages_dockers=None, luma_config=None):
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
        apps_sys_config = cfg['nodes']['node']['sys.config']
        sys_config = apps_sys_config[self.app_name()]

        if 'cluster_worker' not in apps_sys_config:
            apps_sys_config['cluster_worker'] = dict()
        apps_sys_config['cluster_worker']['external_ip'] = {'string': 'IP_PLACEHOLDER'}

        if 'onepanel_rest_url' in sys_config:
            rest_url = sys_config['onepanel_rest_url']
            port = rest_url['port']
            protocol = rest_url['protocol']
            node_name, _sep, instance = rest_url['domain'].partition('.')
            panel_hostname = panel.panel_hostname(node_name, instance, uid)
            sys_config["onepanel_rest_url"] = {
                'string': "{0}://{1}:{2}".format(protocol, panel_hostname, port)
            }

        return cfg

    # Called BEFORE the instance (cluster of workers) is started,
    # once for every instance
    def pre_configure_instance(self, instance, instance_domain, config):
        this_config = config[self.domains_attribute()][instance]
        if 'gui_override' in this_config and isinstance(
                this_config['gui_override'], dict):
            # Preconfigure GUI override
            gui_config = this_config['gui_override']
            gui.override_gui(gui_config, instance_domain)

    # Called AFTER the instance (cluster of workers) has been started
    def post_configure_instance(self, bindir, instance, config, container_ids,
                                output, storages_dockers=None,
                                luma_config=None):
        this_config = config[self.domains_attribute()][instance]
        # Check if gui livereload is enabled in env and turn it on
        if 'gui_override' in this_config and isinstance(
                this_config['gui_override'], dict):
            gui_config = this_config['gui_override']
            livereload_flag = gui_config['livereload']
            if livereload_flag:
                for container_id in container_ids:
                    livereload_dir = gui_config['mount_path']
                    gui.run_livereload(container_id, livereload_dir)

    def pre_start_commands(self, domain):
        return '''
sed -i.bak s/\"IP_PLACEHOLDER\"/\"`ip addr show eth0 | grep "inet\\b" | awk '{{print $2}}' | cut -d/ -f1`\"/g /tmp/gen_dev_args.json
escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json
mkdir -p /root/bin/node/data/
touch /root/bin/node/data/dns.config
sed -i.bak s/onedata.org/{domain}/g /root/bin/node/data/dns.config
        '''.format(domain=domain)

    def extra_volumes(self, config, bindir, instance_domain, storages_dockers):
        extra_volumes = []
        # Check if gui override is enabled in env and add required volumes
        if 'gui_override' in config and isinstance(config['gui_override'],
                                                   dict):
            gui_config = config['gui_override']
            extra_volumes.extend(gui.extra_volumes(gui_config, instance_domain))
        return extra_volumes

    def couchbase_ramsize(self):
        return 1124

    def couchbase_buckets(self):
        return {"onedata": 1024}

    def app_name(self):
        return "oz_worker"

    def domains_attribute(self):
        return "zone_domains"

    def domain_env_name(self):
        return "http_domain"

    def nodes_list_attribute(self):
        return "oz_worker_nodes"

    def has_dns_server(self):
        return True
