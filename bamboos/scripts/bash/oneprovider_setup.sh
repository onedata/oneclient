#!/bin/sh

#####################################################################
# @author Krzysztof Trzepla
# @copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script is used by Bamboo agent to set up oneprovider nodes
# during deployment.
#####################################################################

#####################################################################
# platform initialization
#####################################################################

source functions.sh || exit 1

info "Fetching platform configuration from $MASTER:$CONFIG_PATH ..."
scp ${MASTER}:${CONFIG_PATH} onedata_platform.cfg || error "Cannot fetch platform configuration file."
eval `escript config_setup.escript onedata_platform.cfg --source_all` || error "Cannot parse platform configuration file."

#####################################################################
# platform clean-up
#####################################################################

ALL_NODES="$ONEPROVIDER_NODES ; $ONEPROVIDER_DB_NODES"
ALL_NODES=`echo ${ALL_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
for node in ${ALL_NODES}; do

    [[
        "$node" != ""
    ]] || continue

    remove_oneprovider   "$node"

    ssh ${node} "rm -rf $SETUP_DIR"
    ssh ${node} "killall -KILL beam 2> /dev/null"
    ssh ${node} "killall -KILL beam.smp 2> /dev/null"
done

#####################################################################
# configuration validation
#####################################################################

if [[ `len "$ONEPROVIDER_NODES"` == 0 ]]; then
    error "oneprovider nodes are not configured!"
fi

if [[ `len "$ONEPROVIDER_DB_NODES"` == 0 ]]; then
    error "oneprovider DB nodes are not configured!"
fi

#####################################################################
# oneprovider package installation
#####################################################################

ALL_NODES="$ONEPROVIDER_NODES ; $ONEPROVIDER_DB_NODES"
ALL_NODES=`echo ${ALL_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
for node in ${ALL_NODES}; do
    [[
        "$node" != ""
    ]] || error "Invalid oneprovider node!"

    ssh ${node} "mkdir -p $SETUP_DIR" || error "Cannot create tmp setup dir '$SETUP_DIR' on ${node}"
    install_package ${node} oneprovider.rpm
done

#####################################################################
# registration in globalregistry setup
#####################################################################

if [[ "$ONEPROVIDER_ID" != "" ]]; then

    if [[ `len "$GLOBALREGISTRY_NODES"` == 0 ]]; then
        error "globalregistry nodes are not configured!"
    fi

    globalregistry_node=`nth "$GLOBALREGISTRY_NODES" 1`
    globalregistry_ip=`strip_login ${globalregistry_node}`

    ALL_NODES=`echo ${ONEPROVIDER_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
    for node in ${ALL_NODES}; do
        ssh ${node} "sed -i -e '/onedata.*/d' /etc/hosts" || error "Cannot remove old mappings from /etc/hosts for onedata domain on $node"
        ssh ${node} "echo \"$globalregistry_ip    onedata.org\" >> /etc/hosts" || error "Cannot set new mappings in /etc/hosts for onedata domain on $node"
    done
fi

#####################################################################
# oneprovider nodes start
#####################################################################

node=`nth "$ONEPROVIDER_NODES" 1`
start_oneprovider ${node}

n_count=`len "$ONEPROVIDER_NODES"`
for i in `seq 1 ${n_count}`; do
    node=`nth "$ONEPROVIDER_NODES" ${i}`

    deploy_stamp ${node}
done

#####################################################################
# oneprovider database initialization
#####################################################################

info "Initializing oneprovider database..."

node=`nth "$GLOBALREGISTRY_NODES" 1`
ssh ${node} "mkdir -p ${SETUP_DIR}"
scp globalregistry_setup.escript ${node}:${SETUP_DIR} || error "Cannot copy globalregistry setup escript to ${node}."

node=`nth "$ONEPROVIDER_NODES" 1`
scp onedata_platform.cfg ${node}:${SETUP_DIR} || error "Cannot copy platform configuration file to ${node}."
scp oneprovider_setup.escript ${node}:${SETUP_DIR} || error "Cannot copy oneprovider setup escript to ${node}."
escript_bin=`ssh ${node} "find /opt/oneprovider/files/oneprovider_node/ -name escript | head -1"` || error "Cannot find escript binary on ${node}"
ssh ${node} "${escript_bin} ${SETUP_DIR}/oneprovider_setup.escript --initialize ${SETUP_DIR}/onedata_platform.cfg ${SETUP_DIR}" || error "Cannot initialize oneprovider database on ${node}"

exit 0