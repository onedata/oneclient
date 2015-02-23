#!/bin/sh

#####################################################################
# @author Krzysztof Trzepla
# @copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script is used by Bamboo agent to set up oneclient nodes
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
# configuration validation
#####################################################################

if [[ `len "$ONECLIENT_NODES"` == 0 ]]; then
    error "oneclient nodes are not configured!"
fi

if [[ `len "$ONECLIENT_MOUNTS"` == 0 ]]; then
    error "oneclient mount points are not configured!"
fi

if [[ `len "$ONECLIENT_CERTS"` == 0 ]]; then
    error "oneclient certificates are not configured!"
fi

#####################################################################
# oneclient nodes setup
#####################################################################

n_count=`len "$ONECLIENT_NODES"`
for i in `seq 1 ${n_count}`; do
    node=`nth "$ONECLIENT_NODES" ${i}`
    mount=`nth "$ONECLIENT_MOUNTS" ${i}`
    cert=`nth "$ONECLIENT_CERTS" ${i}`

    [[
        "$node" != "" &&
        "$mount" != "" &&
        "$cert" != ""
    ]] || error "Invalid oneclient node!"
    
    echo "Processing oneclient on node '$node' with mountpoint '$mount' and certificate '$cert'..."
    
    remove_oneclient "$node" "$mount"
    install_oneclient "$node" "$mount" "$cert"
done

#####################################################################
# oneclient nodes start
#####################################################################

n_count=`len "$ONECLIENT_NODES"`
for i in `seq 1 ${n_count}`; do
    id=`nth "$ONECLIENT_USERS" ${i}`
    node=`nth "$ONECLIENT_NODES" ${i}`
    mount=`nth "$ONECLIENT_MOUNTS" ${i}`
    cert=`nth "$ONECLIENT_CERTS" ${i}`
    auth=`nth "$ONECLIENT_AUTH" ${i}`
  
    start_oneclient "$id" "$node" "$mount" "$cert" "$auth" "$i"
    deploy_stamp "$node"
done

exit 0