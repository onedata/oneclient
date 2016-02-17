"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider worker nodes. They can create separate clusters.
"""

import os
import subprocess
import sys
from . import common, docker, worker, globalregistry, gui_livereload

DOCKER_BINDIR_PATH = '/root/build'


def up(image, bindir, dns_server, uid, config_path, logdir=None):
    return worker.up(image, bindir, dns_server, uid, config_path,
                     ProviderWorkerConfigurator(), logdir)


class ProviderWorkerConfigurator:
    def tweak_config(self, cfg, uid):
        sys_config = cfg['nodes']['node']['sys.config'][self.app_name()]
        if 'global_registry_domain' in sys_config:
            gr_hostname = globalregistry.gr_domain(
                sys_config['global_registry_domain'], uid)
            sys_config['global_registry_domain'] = gr_hostname
        return cfg

    def configure_started_instance(self, bindir, instance, config,
                                   container_ids, output):
        this_config = config[self.domains_attribute()][instance]
        # Check if gui_livereload is enabled in env and turn it on
        if 'gui_livereload' in this_config:
            if this_config['gui_livereload']:
                print 'Starting GUI livereload for provider {0}.'.format(
                    instance)
                for container_id in container_ids:
                    gui_livereload.run(
                        container_id,
                        os.path.join(bindir, 'rel/gui.config'),
                        DOCKER_BINDIR_PATH,
                        '/root/bin/node')
        if 'os_config' in this_config:
            os_config = this_config['os_config']
            create_storages(config['os_configs'][os_config]['storages'],
                            output[self.nodes_list_attribute()],
                            this_config[self.app_name()], bindir)

    def extra_volumes(self, config, bindir):
        storage_volumes = [common.volume_for_storage(s) for s in config[
            'os_config']['storages']] if 'os_config' in config else []
        # Check if gui_livereload is enabled in env and add required storages
        if 'gui_livereload' in config:
            if config['gui_livereload']:
                storage_volumes += gui_livereload.required_volumes(
                    os.path.join(bindir, 'rel/gui.config'),
                    bindir,
                    DOCKER_BINDIR_PATH)
        return storage_volumes

    def app_name(self):
        return "op_worker"

    def domains_attribute(self):
        return "provider_domains"

    def domain_env_name(self):
        return "provider_domain"

    def nodes_list_attribute(self):
        return "op_worker_nodes"


def create_storages(storages, op_nodes, op_config, bindir):
    # copy escript to docker host
    script_name = 'create_storage.escript'
    pwd = common.get_script_dir()
    command = ['cp', os.path.join(pwd, script_name),
               os.path.join(bindir, script_name)]
    subprocess.check_call(command)
    # execute escript on one of the nodes
    # (storage is common fo the whole provider)
    first_node = op_nodes[0]
    container = first_node.split("@")[1]
    worker_name = container.split(".")[0]
    cookie = op_config[worker_name]['vm.args']['setcookie']
    script_path = os.path.join(DOCKER_BINDIR_PATH, script_name)
    for st_path in storages:
        st_name = st_path
        command = ['escript', script_path, cookie, first_node, st_name, st_path]
        assert 0 is docker.exec_(container, command, tty=True,
                                 stdout=sys.stdout, stderr=sys.stderr)
    # clean-up
    command = ['rm', os.path.join(bindir, script_name)]
    subprocess.check_call(command)
