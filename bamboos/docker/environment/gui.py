# coding=utf-8
"""Author: Lukasz Opiola
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Utility module to handle gui deployment on dockers and livereload of Ember GUI
(for development purposes).
"""

import os
from . import common, docker


def override_gui(gui_config, instance_domain):
    print('GUI override for: {0}'.format(instance_domain))
    # Prepare static dockers with GUI (if required) - they are reused by
    # whole cluster (instance)
    mount_path = gui_config['mount_path']
    print('    static root: {0}'.format(mount_path))
    if 'host' in gui_config['mount_from']:
        host_volume_path = gui_config['mount_from']['host']
        print('    from host:   {0}'.format(host_volume_path))
    elif 'docker' in gui_config['mount_from']:
        static_docker_image = gui_config['mount_from']['docker']
        # Create volume name from docker image name and instance domain
        volume_name = gui_files_volume_name(
            static_docker_image, instance_domain)
        # Create the volume from given image
        docker.create_volume(
            path=mount_path,
            name=volume_name,
            image=static_docker_image,
            command='/bin/true')
        print('    from docker: {0}'.format(static_docker_image))
    livereload_flag = gui_config['livereload']
    print('    livereload:  {0}'.format(livereload_flag))


def extra_volumes(gui_config, instance_domain):
    if 'host' in gui_config['mount_from']:
        # Mount a path on host to static root dir on OZ docker
        mount_path = gui_config['mount_path']
        host_volume_path = gui_config['mount_from']['host']
        return [(host_volume_path, mount_path, 'ro')]
    elif 'docker' in gui_config['mount_from']:
        static_docker_image = gui_config['mount_from']['docker']
        # Create volume name from docker image name
        volume_name = gui_files_volume_name(
            static_docker_image, instance_domain)
        return [{'volumes_from': volume_name}]


def run_livereload(container_id, dir_to_watch, detach=True):
    """
    Starts a process on given docker that monitors changes in GUI static root
    and forces a page reload using websocket connection to client.
    The WS connection is created when livereload script is injected on the page
    - this must be done from the Ember client app.
    If automatic rebuilding of GUI is required, it should be run independently and
    should place compiled files in the static root dir which was given
    as dir_to_watch.
    """

    # Copy the js script that start livereload and a cert that will
    # allow to start a https server for livereload, and start livereload.
    command = '''\
mkdir -p /tmp/gui_livereload
cd /tmp/gui_livereload
cat <<"EOF" > /tmp/gui_livereload/cert.pem
{gui_livereload_cert}
EOF
npm link livereload
cat <<"EOF" > gui_livereload.js
{gui_livereload}
EOF
node gui_livereload.js {dir_to_watch} poll /tmp/gui_livereload/cert.pem'''
    js_path = os.path.join(common.get_script_dir(), 'gui_livereload.js')
    cert_path = os.path.join(common.get_script_dir(), 'gui_livereload_cert.pem')
    command = command.format(
        dir_to_watch=dir_to_watch,
        gui_livereload_cert=open(cert_path, 'r').read(),
        gui_livereload=open(js_path, 'r').read())

    return docker.exec_(
        container=container_id,
        detach=detach,
        interactive=True,
        tty=True,
        command=command,
        output=True)


# Create volume name from docker image name and instance domain
def gui_files_volume_name(image_name, instance_domain):
    volume_name = image_name.split('/')[-1].replace(
        ':', '-').replace('_', '-').replace('.', '-')
    return '{0}-{1}'.format(volume_name, instance_domain)
