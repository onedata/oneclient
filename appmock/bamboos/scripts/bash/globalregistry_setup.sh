#!/bin/sh

#####################################################################
# @author Krzysztof Trzepla
# @copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script is used by Bamboo agent to set up globalregistry nodes
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

if [[ "$PLATFORM_CLEANUP" == "yes" ]]; then

    ALL_NODES="$ONEPROVIDER_NODES ; $ONEPROVIDER_DB_NODES ; $GLOBALREGISTRY_NODES ; $GLOBALREGISTRY_DB_NODES ; $ONECLIENT_NODES"
    ALL_NODES=`echo ${ALL_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
    for node in ${ALL_NODES}; do

        [[
            "$node" != ""
        ]] || continue

        remove_oneprovider "$node"
        remove_globalregistry "$node"

        ssh ${node} "rm -rf $SETUP_DIR"
        ssh ${node} "killall -KILL beam 2> /dev/null"
        ssh ${node} "killall -KILL beam.smp 2> /dev/null"
    done
else

    ALL_NODES="$GLOBALREGISTRY_NODES ; $GLOBALREGISTRY_DB_NODES"
    ALL_NODES=`echo ${ALL_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
    for node in ${ALL_NODES}; do

        [[
            "$node" != ""
        ]] || continue

        remove_globalregistry "$node"

        ssh ${node} "rm -rf $SETUP_DIR"
        ssh ${node} "killall -KILL beam 2> /dev/null"
        ssh ${node} "killall -KILL beam.smp 2> /dev/null"
    done
fi

#####################################################################
# configuration validation
#####################################################################

if [[ `len "$GLOBALREGISTRY_NODES"` == 0 ]]; then
    error "globalregistry nodes are not configured!"
fi

if [[ `len "$GLOBALREGISTRY_DB_NODES"` == 0 ]]; then
    error "globalregistry DB nodes are not configured!"
fi

#####################################################################
# globalregistry package installation
#####################################################################

ALL_NODES="$GLOBALREGISTRY_NODES ; $GLOBALREGISTRY_DB_NODES"
ALL_NODES=`echo ${ALL_NODES} | tr ";" "\n" | sed -e 's/^ *//g' -e 's/ *$//g' | sort | uniq`
for node in ${ALL_NODES}; do
    [[
        "$node" != ""
    ]] || error "Invalid globalregistry node!"

    ssh ${node} "mkdir -p $SETUP_DIR" || error "Cannot create tmp setup dir '$SETUP_DIR' on ${node}"
    install_package ${node} globalregistry.rpm
done

#####################################################################
# globalregistry nodes start
#####################################################################

node=`nth "$GLOBALREGISTRY_NODES" 1`
start_globalregistry "$node"

#####################################################################
# globalregistry database initialization
#####################################################################

info "Initializing globalregistry database..."

node=`nth "$GLOBALREGISTRY_NODES" 1`
scp onedata_platform.cfg ${node}:${SETUP_DIR} || error "Cannot copy platform configuration file to ${node}."
scp globalregistry_setup.escript ${node}:${SETUP_DIR} || error "Cannot copy globalregistry setup escript to ${node}."
escript_bin=`ssh ${node} "find /opt/globalregistry -name escript | head -1"` || error "Cannot find escript binary on ${node}"
ssh ${node} "${escript_bin} ${SETUP_DIR}/globalregistry_setup.escript --initialize ${SETUP_DIR}/onedata_platform.cfg ${SETUP_DIR}" || error "Cannot initialize globalregistry database on ${node}"

exit 0