"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of oneprovider worker nodes. They can create separate clusters.
"""

import os
import subprocess
import sys
from . import common, docker, worker, gui


def up(image, bindir, dns_server, uid, config_path, logdir=None,
       storages_dockers=None, luma_config=None):
    return worker.up(image, bindir, dns_server, uid, config_path,
                     ProviderWorkerConfigurator(), logdir,
                     storages_dockers=storages_dockers,
                     luma_config=luma_config)


class ProviderWorkerConfigurator:
    def tweak_config(self, cfg, uid, instance):
        sys_config = cfg['nodes']['node']['sys.config'][self.app_name()]
        if 'oz_domain' in sys_config:
            oz_hostname = worker.cluster_domain(sys_config['oz_domain'], uid)
            sys_config['oz_domain'] = oz_hostname
        return cfg

    def pre_start_commands(self, domain):
        return 'escript bamboos/gen_dev/gen_dev.escript /tmp/gen_dev_args.json'

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
        if 'os_config' in this_config:
            os_config = this_config['os_config']
            create_storages(config['os_configs'][os_config]['storages'],
                            output[self.nodes_list_attribute()],
                            this_config[self.app_name()], bindir,
                            storages_dockers)

    def extra_volumes(self, config, bindir, instance, storages_dockers):
        if 'os_config' in config and config['os_config']['storages']:
            if isinstance(config['os_config']['storages'][0], basestring):
                posix_storages = config['os_config']['storages']
            else:
                posix_storages = [s['name'] for s in
                                  config['os_config']['storages']
                                  if s['type'] == 'posix']
        else:
            posix_storages = []

        extra_volumes = []
        for s in posix_storages:
            if not (storages_dockers and s in storages_dockers['posix'].keys()):
                v = common.volume_for_storage(s)
                (host_path, docker_path, mode) = v
                if not storages_dockers:
                    storages_dockers = {'posix': {}}
                storages_dockers['posix'][s] = {"host_path": host_path, "docker_path": docker_path}
            else:
                d = storages_dockers['posix'][s]
                v = (d['host_path'], d['docker_path'], 'rw')

            extra_volumes.append(v)

        # Check if gui override is enabled in env and add required volumes
        if 'gui_override' in config and isinstance(config['gui_override'],
                                                   dict):
            gui_config = config['gui_override']
            extra_volumes.extend(gui.extra_volumes(gui_config, instance))
        return extra_volumes

    def couchbase_ramsize(self):
        return 1024

    def couchbase_buckets(self):
        return {"default": 512}

    def app_name(self):
        return "op_worker"

    def domains_attribute(self):
        return "provider_domains"

    def domain_env_name(self):
        return "provider_domain"

    def nodes_list_attribute(self):
        return "op_worker_nodes"


def create_storages(storages, op_nodes, op_config, bindir, storages_dockers):
    # copy escript to docker host
    script_names = {'posix': 'create_posix_storage.escript',
                    's3': 'create_s3_storage.escript',
                    'ceph': 'create_ceph_storage.escript',
                    'swift': 'create_swift_storage.escript'}
    pwd = common.get_script_dir()
    for script_name in script_names.values():
        command = ['cp', os.path.join(pwd, script_name),
                   os.path.join(bindir, script_name)]
        subprocess.check_call(command)
    # execute escript on one of the nodes
    # (storage is common fo the whole provider)
    first_node = op_nodes[0]
    container = first_node.split("@")[1]
    worker_name = container.split(".")[0]
    cookie = op_config[worker_name]['vm.args']['setcookie']
    bindir = os.path.abspath(bindir)
    script_paths = dict(
        map(lambda (k, v): (k, os.path.join(bindir, v)),
            script_names.iteritems()))
    for storage in storages:
        if isinstance(storage, basestring):
            storage = {'type': 'posix', 'name': storage}
        if storage['type'] in ['posix', 'nfs']:
            st_path = storage['name']
            command = ['escript', script_paths['posix'], cookie,
                       first_node, storage['name'], st_path]
            assert 0 is docker.exec_(container, command, tty=True,
                                     stdout=sys.stdout, stderr=sys.stderr)
        elif storage['type'] == 'ceph':
            config = storages_dockers['ceph'][storage['name']]
            pool = storage['pool'].split(':')[0]
            command = ['escript', script_paths['ceph'], cookie,
                       first_node, storage['name'], "ceph",
                       config['host_name'], pool, config['username'],
                       config['key']]
            assert 0 is docker.exec_(container, command, tty=True,
                                     stdout=sys.stdout, stderr=sys.stderr)
        elif storage['type'] == 's3':
            config = storages_dockers['s3'][storage['name']]
            command = ['escript', script_paths['s3'], cookie,
                       first_node, storage['name'], config['host_name'],
                       config.get('scheme', 'http'), storage['bucket'],
                       config['access_key'], config['secret_key'],
                       config.get('iam_request_scheme', 'https'),
                       config.get('iam_host', 'iam.amazonaws.com')]
            assert 0 is docker.exec_(container, command, tty=True,
                                     stdout=sys.stdout, stderr=sys.stderr)

        elif storage['type'] == 'swift':
            config = storages_dockers['swift'][storage['name']]
            command = ['escript', script_paths['swift'], cookie,
                       first_node, storage['name'],
                       'http://{0}:{1}/v2.0/tokens'.format(config['host_name'],
                                                    config['keystone_port']),
                       storage['container'], config['tenant_name'],
                       config['user_name'], config['password']]
            assert 0 is docker.exec_(container, command, tty=True,
                                     stdout=sys.stdout, stderr=sys.stderr)

        else:
            raise RuntimeError(
                'Unknown storage type: {}'.format(storage['type']))
    # clean-up
    for script_name in script_names.values():
        command = ['rm', os.path.join(bindir, script_name)]
        subprocess.check_call(command)
