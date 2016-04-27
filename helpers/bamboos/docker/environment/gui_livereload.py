# coding=utf-8
"""Author: Lukasz Opiola
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Utility module to handle livereload of Ember GUI (for development purposes) on
dockers.

It can work in following modes:
    'watch' - uses FS watcher, does not work on network filesystems.
        Runs entirely on docker.

    'poll' - polls for changes, very slow but works everywhere.
        Runs entirely on docker.

    'mount_output' - mounts GUI output path in docker so if any changes are made
        on the host, they appear on docker. The dir is used by cowboy as static
        files dir (proper changes are automatically made to sys.config),
        so new files will be served on page reload.
        This allows to run 'ember build --watch' on host machine.

    'mount_output_poll' - like above, but it also starts a livereload server
        on docker that will poll for changes in the mounted output dir and force
        a reload of the page (via websocket) whenever something changes.

    'none' - doesn't start gui livereload.
"""

import os
import re
import sys
from . import common, docker


def get_output_mount_path():
    """
    Indicates where on docker should gui output dir be mounted.
    """
    return '/root/gui_static'


def assert_correct_mode(mode):
    """
    Makes sure that given livereload mode is correct.
    """
    if mode not in [
        'poll', 'watch', 'mount_output', 'mount_output_poll', 'none'
    ]:
        sys.stderr.write('''\
Unknown livereload mode: `{0}`. Use one of the following:
    watch
    poll
    mount_output
    mount_output_poll
    none
'''.format(mode))
        sys.exit(1)


def required_volumes(gui_config_file,
                     project_src_dir, project_target_dir,
                     docker_src_dir,
                     mode='watch'):
    """
    Returns volumes that are required for livereload to work based on:
    gui_config_file - config file, source_gui_dir is resolved from it
    project_src_dir - path on host to project root
    docker_src_dir - path on docker to project root
    See file header for possible modes.
    """
    assert_correct_mode(mode)

    source_gui_dir = _parse_erl_config(gui_config_file, 'source_gui_dir')
    release_gui_dir = _parse_erl_config(gui_config_file, 'release_gui_dir')
    if mode == 'poll' or mode == 'watch':
        return [
            (
                os.path.join(project_src_dir, source_gui_dir),
                os.path.join(docker_src_dir, source_gui_dir),
                'rw'
            )
        ]
    elif mode == 'mount_output' or mode == 'mount_output_poll':
        return [
            (
                os.path.join(project_src_dir, project_target_dir,
                             release_gui_dir),
                os.path.join(get_output_mount_path()),
                'ro'
            )
        ]
    else:
        return []


def run(container_id,
        gui_config_file,
        project_target_dir,
        docker_src_dir, docker_bin_dir,
        mode='watch'):
    """
    Runs automatic rebuilding of project and livereload of web pages when
    their code changes.
    See file header for possible modes.
    """
    assert_correct_mode(mode)

    # Resolve full paths to gui sources
    source_gui_dir = _parse_erl_config(gui_config_file, 'source_gui_dir')
    source_gui_dir = os.path.join(docker_src_dir, source_gui_dir)
    release_gui_dir = _parse_erl_config(gui_config_file, 'release_gui_dir')
    release_gui_dir = os.path.join(docker_bin_dir, release_gui_dir)

    if mode == 'watch' or mode == 'poll':
        watch_changes(container_id, source_gui_dir, release_gui_dir, mode=mode)
        start_livereload(container_id, release_gui_dir, mode=mode)

    elif mode == 'mount_output_poll':
        # print(container_id)
        # print(get_output_mount_path())
        # print(mode)
        start_livereload(container_id, get_output_mount_path(), mode='poll')


def watch_changes(container_id,
                  source_gui_dir, release_gui_dir,
                  mode='watch', detach=True):
    """
    Starts a process on given docker that monitors changes in GUI sources and
    rebuilds the project when something changes.
    source_gui_dir and release_gui_dir are paths on docker.
    Mode can be 'watch' or 'poll', see file header for more.
    """
    assert_correct_mode(mode)

    source_tmp_dir = os.path.join(source_gui_dir, 'tmp')

    watch_option = '--watch'
    if mode == 'poll':
        watch_option = '--watch --watcher=polling'

    # Start a process that will chown ember tmp dir
    # (so that it does not belong to root afterwards)
    command = '''\
mkdir -p /root/bin/
mkdir -p {source_tmp_dir}
chown -R {uid}:{gid} {source_tmp_dir}
echo 'while ((1)); do chown -R {uid}:{gid} {source_tmp_dir}; sleep 1; done' > /root/bin/chown_tmp_dir.sh
chmod +x /root/bin/chown_tmp_dir.sh
nohup bash /root/bin/chown_tmp_dir.sh &
cd {source_gui_dir}
ember build {watch_option} --output-path={release_gui_dir} > /tmp/ember_build.log 2>&1'''
    command = command.format(
        uid=os.geteuid(),
        gid=os.getegid(),
        source_tmp_dir=source_tmp_dir,
        source_gui_dir=source_gui_dir,
        watch_option=watch_option,
        release_gui_dir=release_gui_dir)

    return docker.exec_(
        container=container_id,
        detach=detach,
        interactive=True,
        tty=True,
        command=command,
        output=True)


def start_livereload(container_id, release_gui_dir, mode='watch', detach=True):
    """
    Starts a process on given docker that monitors changes in GUI release and
    forces a page reload using websocket connection to client. The WS connection
    is created when livereload script is injected on the page - this must be
    done from the Ember client app.
    source_gui_dir and release_gui_dir are paths on docker.
    Mode can be 'watch' or 'poll', see file header for more.
    """
    assert_correct_mode(mode)

    watch_option = 'watch'
    if mode == 'poll':
        watch_option = 'poll'

    # Copy the js script that start livereload and a cert that will
    # allow to start a https server for livereload
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
node gui_livereload.js {release_gui_dir} {watch_option} /tmp/gui_livereload/cert.pem'''
    js_path = os.path.join(common.get_script_dir(), 'gui_livereload.js')
    cert_path = os.path.join(common.get_script_dir(), 'gui_livereload_cert.pem')
    command = command.format(
        release_gui_dir=release_gui_dir,
        gui_livereload_cert=open(cert_path, 'r').read(),
        gui_livereload=open(js_path, 'r').read(),
        watch_option=watch_option)

    return docker.exec_(
        container=container_id,
        detach=detach,
        interactive=True,
        tty=True,
        command=command,
        output=True)


def _parse_erl_config(file, param_name):
    """
    Parses an erlang-style config file finding a tuple by key and extracting
    the value. The tuple must be in form {key, "value"} (and in one line).
    """
    file = open(file, 'r')
    file_content = file.read()
    file.close()
    matches = re.findall("{" + param_name + ".*", file_content)
    return matches[0].split('"')[1]
