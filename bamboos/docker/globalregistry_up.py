#!/usr/bin/env python

"""
Brings up a Global Registry cluster along with a database.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import collections
import json
import os
import sys
import time
import docker

def pull_image(name):
    try:
        client.inspect_image(name)
    except docker.errors.APIError:
        print('Pulling image {name}'.format(name=name), file=sys.stderr)
        client.pull(name)


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up globalregistry.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='docker image to use as a container',
    dest='image')

parser.add_argument(
    '--bin', '-b',
    action='store',
    default=os.getcwd(),
    help='path to globalregistry directory (precompiled)',
    dest='bin')

args = parser.parse_args()
cookie = str(int(time.time()))

db_name = 'bigcouch'
db_host = '{0}.{1}.dev.docker'.format(db_name, cookie)
db_dockername = '{0}_{1}'.format(db_name, cookie)
gr_name = 'globalregistry'
gr_host = '{0}.{1}.dev.docker'.format(gr_name, cookie)
gr_dockername = '{0}_{1}'.format(gr_name, cookie)

db_command = \
'''echo '[httpd]' > /opt/bigcouch/etc/local.ini
echo 'bind_address = 0.0.0.0' >> /opt/bigcouch/etc/local.ini
sed -i 's/-name bigcouch/-name {name}@{host}/g' /opt/bigcouch/etc/vm.args
sed -i 's/-setcookie monster/-setcookie {cookie}/g' /opt/bigcouch/etc/vm.args
/opt/bigcouch/bin/bigcouch'''
db_command = db_command.format(name=db_name, host=db_host, cookie=cookie)

gr_config = \
'''{{node, "{grname}@{grhost}"}}.
{{cookie, "{cookie}"}}.
{{db_nodes, "['{dbname}@{dbhost}']"}}.
{{grpcert_domain, "\\"onedata.org\\""}}.'''
gr_config = gr_config.format(dbname=db_name, dbhost=db_host, cookie=cookie,
                             grname=gr_name, grhost=gr_host)

gr_command = \
'''set -e
rsync -rogl /root/bin/ /root/run
cd /root/run
cat <<"EOF" > rel/vars.config
{cfg}
EOF
rm -Rf rel/globalregistry
cat rel/vars.config
./rebar generate
rel/globalregistry/bin/globalregistry console'''
gr_command = gr_command.format(cfg=gr_config)

bigcouch = docker.run(
    image='onedata/bigcouch',
    detach=True,
    name=db_dockername,
    hostname=db_host,
    command=db_command)

gr = docker.run(
    image=args.image,
    hostname=gr_host,
    detach=True,
    interactive=True,
    tty=True,
    name=gr_dockername,
    volumes=[(args.bin, '/root/bin', 'ro')],
    link={db_dockername: db_host},
    command=gr_command)

output = collections.defaultdict(list)
output['docker_ids'] = [bigcouch, gr]
output['gr_db_nodes'] = ['{0}@{1}'.format(db_name, db_host)]
output['gr_worker_nodes'] = ['{0}@{1}'.format(gr_name, gr_host)]

print(json.dumps(output))
