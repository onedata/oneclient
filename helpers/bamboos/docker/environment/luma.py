# coding=utf-8
"""Authors: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a python LUMA.
"""
import ConfigParser
import json
import StringIO

from . import docker, common

default_config = {
    "config": {
        "DEBUG": "True",
        "DATABASE": "'luma_database.db'",
        "HOST": "'0.0.0.0'",
    },
    "generators_config": {
        "ceph": {
            "user": "client.admin",
            "key": "key",
            "mon_host": "",
            "pool_name": "onedata"
        },
        "s3": {
            "access_key": "AccessKey",
            "secret_key": "SecretKey"
        },
        "posix": {
            "lowest_uid": 1000,
            "highest_uid": 65536
        }
    },
    "generators_mapping": [
        {
            "storage_id": "Ceph",
            "generator_id": "ceph"
        },
        {
            "storage_type": "DirectIO",
            "generator_id": "posix"
        },
        {
            "storage_type": "AmazonS3",
            "generator_id": "s3"
        }
    ],
    "storages_mapping": [],
    "credentials_mapping": []
}


def get_default_config():
    return default_config


def _json_to_cfg(json):
    config = ConfigParser.RawConfigParser()
    config.optionxform = str
    for section, data in json.iteritems():
        if section != ConfigParser.DEFAULTSECT:
            config.add_section(section)
        for key, value in data.iteritems():
            config.set(section, key, value)
    output = StringIO.StringIO()
    config.write(output)
    result = output.getvalue()
    output.close()
    return result


def _node_up(image, bindir, config, uid):
    hostname = common.format_hostname('luma', uid)
    volumes = [(bindir, '/root/luma', 'ro')]

    luma_config = default_config
    if config:
        for key in default_config.keys():
            if key in config['luma_setup']:
                luma_config[key] = config['luma_setup'][key]

    command = '''
cp -r /root/luma/* /root/bin
echo "{0}" > /tmp/config.cfg
echo '{1}' > /tmp/generators_mapping.json
echo '{2}' > /tmp/storages_mapping.json
echo '{3}' > /tmp/credentials_mapping.json
rm -f /root/bin/generators/generators.cfg
echo "{4}" > /root/bin/generators/generators.cfg
./init_db.py -c /tmp/config.cfg
./main.py -c /tmp/config.cfg -gm /tmp/generators_mapping.json \
    -cm /tmp/credentials_mapping.json -sm /tmp/storages_mapping.json \
    > /tmp/run.log 2>&1
'''.format(
        # Flask requires configuration without sections
        '\n'.join(
            _json_to_cfg({'DEFAULT': luma_config['config']}).split('\n')[1:]),
        json.dumps(luma_config['generators_mapping']),
        json.dumps(luma_config['storages_mapping']),
        json.dumps(luma_config['credentials_mapping']),
        _json_to_cfg(luma_config['generators_config'])
    )

    container = docker.run(
        image=image,
        detach=True,
        interactive=True,
        tty=True,
        workdir='/root/bin',
        volumes=volumes,
        run_params=["--privileged"],
        command=command, output=True,
        name=hostname, hostname=hostname)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {'docker_ids': [container], 'host_name': ip, 'port': 5000}


def up(image, bindir, config, uid):
    return _node_up(image, bindir, config, uid)
