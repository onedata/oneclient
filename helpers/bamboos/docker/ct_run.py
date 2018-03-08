#!/usr/bin/env python

"""Author: Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs oneprovider integration tests, providing Erlang's ct_run with every
environmental argument it needs for successful run. The output is put into
'test_distributed/logs'. The (init|end)_per_suite "testcases" are removed from
the surefire.xml output.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

from os.path import expanduser
import argparse
import json
import os
import platform
import re
import shutil
import sys
import time
import glob
import xml.etree.ElementTree as ElementTree

sys.path.insert(0, 'bamboos/docker')
from environment import docker
from environment.common import HOST_STORAGE_PATH, remove_dockers_and_volumes


def skipped_test_exists(junit_report_path):
    reports = glob.glob(junit_report_path)
    # if there are many reports, check only the last one
    reports.sort()
    tree = ElementTree.parse(reports[-1])
    testsuites = tree.getroot()
    for testsuite in testsuites:
        if testsuite.attrib['skipped'] != '0':
            return True
    return False


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='docker image to use as a test master',
    dest='image')

parser.add_argument(
    '--suite', '-s',
    action='append',
    help='name of the test suite',
    dest='suites')

parser.add_argument(
    '--case', '-c',
    action='append',
    help='name of the test case',
    dest='cases')

parser.add_argument(
    '--performance', '-p',
    action='store_true',
    default=False,
    help='run performance tests',
    dest='performance')

parser.add_argument(
    '--cover',
    action='store_true',
    default=False,
    help='run cover analysis',
    dest='cover')

parser.add_argument(
    '--stress',
    action='store_true',
    default=False,
    help='run stress tests',
    dest='stress')

parser.add_argument(
    '--stress-no-clearing',
    action='store_true',
    default=False,
    help='run stress tests without clearing data between test cases',
    dest='stress_no_clearing')

parser.add_argument(
    '--stress-time',
    action='store',
    help='time of stress test in sek',
    dest='stress_time')

parser.add_argument(
    '--auto-compile',
    action='store_true',
    default=False,
    help='compile test suites before run',
    dest='auto_compile')

args = parser.parse_args()
script_dir = os.path.dirname(os.path.abspath(__file__))
uid = str(int(time.time()))

excl_mods = glob.glob(
    os.path.join(script_dir, 'test_distributed', '*.erl'))
excl_mods = [os.path.basename(item)[:-4] for item in excl_mods]

cover_template = os.path.join(script_dir, 'test_distributed', 'cover.spec')
new_cover = os.path.join(script_dir, 'test_distributed', 'cover_tmp.spec')

incl_dirs = []
with open(cover_template, 'r') as template, open(new_cover, 'w') as cover:
    for line in template:
        if 'incl_dirs_r' in line:
            dirs_string = re.search(r'\[(.*?)\]', line).group(1)
            incl_dirs = [os.path.join(script_dir, d[1:]) for d in
                         dirs_string.split(', ')]
            docker_dirs = [os.path.join(script_dir, d[1:-1]) for d in
                           dirs_string.split(', ')]
        elif 'excl_mods' in line:
            modules_string = re.search(r'\[(.*?)\]', line).group(1)
            excl_mods.extend([d.strip('"') for d in modules_string.split(', ')])
        else:
            print(line, file=cover)

    print('{{incl_dirs_r, ["{0}]}}.'.format(', "'.join(incl_dirs)), file=cover)
    print('{{excl_mods, [{0}]}}.'.format(
        ', '.join(excl_mods)), file=cover)

ct_command = ['ct_run',
              '-abort_if_missing_suites',
              '-dir', '.',
              '-logdir', './logs/',
              '-ct_hooks', 'cth_surefire', '[{path, "surefire.xml"}]',
              'and', 'cth_logger', 'and', 'cth_env_up', 'and', 'cth_mock',
              'and', 'cth_posthook',
              '-noshell',
              '-name', 'testmaster@testmaster.{0}.test'.format(uid),
              '-include', '../include', '../_build/default/lib']

code_paths = ['-pa']
if incl_dirs:
    code_paths.extend([os.path.join(script_dir, item[:-1])
                       for item in incl_dirs])
code_paths.extend(
    glob.glob(os.path.join(script_dir, '_build/default/lib', '*', 'ebin')))
ct_command.extend(code_paths)

if args.suites:
    ct_command.append('-suite')
    ct_command.extend(args.suites)

if args.cases:
    ct_command.append('-case')
    ct_command.extend(args.cases)

if args.stress_time:
    ct_command.extend(['-env', 'stress_time', args.stress_time])

if args.performance:
    ct_command.extend(['-env', 'performance', 'true'])
elif args.stress:
    ct_command.extend(['-env', 'stress', 'true'])
elif args.stress_no_clearing:
    ct_command.extend(['-env', 'stress_no_clearing', 'true'])
elif args.cover:
    ct_command.extend(['-cover', 'cover_tmp.spec'])
    env_descs = glob.glob(
        os.path.join(script_dir, 'test_distributed', '*', 'env_desc.json'))
    for file in env_descs:
        shutil.copyfile(file, file + '.bak')
        with open(file, 'r') as jsonFile:
            data = json.load(jsonFile)

            configs_to_change = []
            if 'provider_domains' in data:
                for provider in data['provider_domains']:
                    if 'op_worker' in data['provider_domains'][provider]:
                        configs_to_change.append(
                            ('op_worker', data['provider_domains'][provider][
                                'op_worker'].values())
                        )
                    if 'cluster_manager' in data['provider_domains'][provider]:
                        configs_to_change.append(
                            ('cluster_manager',
                             data['provider_domains'][provider][
                                 'cluster_manager'].values())
                        )

            if 'cluster_domains' in data:
                for cluster in data['cluster_domains']:
                    if 'cluster_worker' in data['cluster_domains'][cluster]:
                        configs_to_change.append(
                            ('cluster_worker', data['cluster_domains'][cluster][
                                'cluster_worker'].values())
                        )
                    if 'cluster_manager' in data['cluster_domains'][cluster]:
                        configs_to_change.append(
                            ('cluster_manager',
                             data['cluster_domains'][cluster][
                                 'cluster_manager'].values())
                        )

            if 'zone_domains' in data:
                for zone in data['zone_domains']:
                    configs_to_change.append(
                        ('oz_worker',
                         data['zone_domains'][zone]['oz_worker'].values())
                    )
                    configs_to_change.append(
                        ('oz_worker',
                         data['zone_domains'][zone]['cluster_manager'].values())
                    )

            if 'onepanel_domains' in data:
                for onepanel in data['onepanel_domains']:
                    configs_to_change.append(
                        ('onepanel', data['onepanel_domains'][onepanel][
                            'onepanel'].values())
                    )

            for (app_name, configs) in configs_to_change:
                for config in configs:
                    if app_name in config['sys.config']:
                        config['sys.config'][app_name][
                            'covered_dirs'] = docker_dirs
                        config['sys.config'][app_name][
                            'covered_excluded_modules'] = excl_mods
                    elif 'cluster_manager' in config['sys.config']:
                        config['sys.config']['cluster_manager'][
                            'covered_dirs'] = docker_dirs
                        config['sys.config']['cluster_manager'][
                            'covered_excluded_modules'] = excl_mods
                    else:
                        config['sys.config']['covered_dirs'] = docker_dirs
                        config['sys.config'][
                            'covered_excluded_modules'] = excl_mods

            with open(file, 'w') as jsonFile:
                jsonFile.write(json.dumps(data))

command = '''
import os, shutil, subprocess, sys, stat

os.environ['HOME'] = '/root'

docker_home = '/root/.docker/'
if {shed_privileges}:
    useradd = ['useradd', '--create-home', '--uid', '{uid}', 'maketmp']

    subprocess.call(useradd)

    os.environ['PATH'] = os.environ['PATH'].replace('sbin', 'bin')
    os.environ['HOME'] = '/home/maketmp'
    docker_home = '/home/maketmp/.docker'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

# Try to copy config.json, continue if it fails (might not exist on host).
try:
    os.makedirs(docker_home)
except:
    pass
try:
    shutil.copyfile(
        '/tmp/docker_config/config.json',
        os.path.join(docker_home, 'config.json'
    ))
except:
    pass

command = {cmd}
ret = subprocess.call(command)

import xml.etree.ElementTree as ElementTree, glob, re
for file in glob.glob('logs/*/surefire.xml'):
    tree = ElementTree.parse(file)
    for suite in tree.findall('.//testsuite'):
        for test in suite.findall('testcase'):
            match = re.match('(init|end)_per_suite', test.attrib['name'])
            if match is not None:
                suite.remove(test)
    tree.write(file)

sys.exit(ret)
'''
command = command.format(
    uid=os.geteuid(),
    gid=os.getegid(),
    cmd=ct_command,
    shed_privileges=(platform.system() == 'Linux'))

volumes = []
if os.path.isdir(expanduser('~/.docker')):
    volumes += [(expanduser('~/.docker'), '/tmp/docker_config', 'ro')]

remove_dockers_and_volumes()

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 workdir=os.path.join(script_dir, 'test_distributed'),
                 volumes=volumes,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw'),
                          (HOST_STORAGE_PATH, 'rw')],
                 name='testmaster_{0}'.format(uid),
                 hostname='testmaster.{0}.test'.format(uid),
                 image=args.image,
                 command=['python', '-c', command])

os.remove(new_cover)
if args.cover:
    for file in env_descs:
        os.remove(file)
        shutil.move(file + '.bak', file)

if ret != 0 and not skipped_test_exists("test_distributed/logs/*/surefire.xml"):
    ret = 0

sys.exit(ret)
