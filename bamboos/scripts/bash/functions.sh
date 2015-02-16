#!/bin/sh

#####################################################################
# @author Rafal Slota
# @copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script contains utility function used by Bamboo agents to
# build and deploy onedata project's components.
#####################################################################

#####################################################################
# platform initialization
#####################################################################

if [[ -z "$CONFIG_PATH" ]]; then
    export CONFIG_PATH="/etc/onedata_platform.conf"
fi

if [[ -z "$SETUP_DIR" ]]; then
    export SETUP_DIR="/tmp/onedata"
fi

if [[ -z "$STAMP_DIR" ]]; then
    export STAMP_DIR="${SETUP_DIR}-stamp"
fi

ONEPANEL_MULTICAST_ADDRESS="239.255.`echo ${MASTER} | awk -F '.' '{print $3}'`.`echo ${MASTER} | awk -F '.' '{print $4}'`"

#####################################################################
# utility functions
#####################################################################

# $1 - list of items seperated with ';'
# $2 - which element has to be returned, starting with 1
function nth {
    echo "$1" | awk -F ';' "{print \$$2}" | xargs
}

# $1 - list of items seperated with ';'
function len {
    echo "$1" | awk -F ';' '{print NF}'    
}

# $1 - error string
function error {
    echo "ERROR [Node: $node]: $1" 1>&2
    exit 1
}

function get_project_name {
    echo ${PROJECT// /_}
}

# $1 - node hostname
function stamp_to_version {
    echo `ssh $1 "cat '$STAMP_DIR/$(get_project_name).stamp'"`
}

# $1 - Branch name
# $2 - Build number
# $3 - Major ver.
# $4 - Minor ver.
function set_version {
    if [[ "$1" == "master" ]]; then
        export V_MAJOR=$3
        export V_MINOR=$4
        export V_PATCH=$2
	      export V_PATCH_ORIG=${V_PATCH}
    else
        export V_MAJOR=0
        export V_MINOR=0
        export V_PATCH="\\\\\"$1\\\\\""
	      export V_PATCH_ORIG="${2}_${1}"
    fi
}

# $1 - node hostname
function deploy_stamp {
    [[
        "$PROJECT" == ""
    ]] || ssh $1 "mkdir -p $STAMP_DIR && echo '$VERSION' > '$STAMP_DIR/`get_project_name`.stamp'"
}

# $1 - info string
function info {
    echo "---> [Node: $node] $1"
}

# $1 - node
# $2 - relative path
function absolute_path_on_node {
    ssh $1 "cd $2 && pwd"
}

# $1 - process name
# $2 - number of seconds
function screen_wait {
    i=$2
    while i; do
        if screen -list | grep "$1"; then
            sleep 1
        else 
            break
        fi
        
        i=$(( i-1 ))
    done
}

# $1 - list of hosts seperated with ';'
# $2 - name of which node has to be returned, starting with 1
function nth_node_name {
    local node=$(nth "$1" $2)
    echo $(ssh ${node} "hostname -f")
}

# $1 - target host
function node_name {
    echo `ssh $1 "hostname -f"`
}

# $1 - target host
function strip_login {
    echo $1 | sed 's/^[^@]*@//'
}

# $1 - target host
# $2 - rpm name
function install_package {
    info "Moving $2 package to $1..."
    scp *.rpm $1:${SETUP_DIR}/$2 || error "Moving $2 file failed on $1"

    info "Installing $2 package on $1..."
    ssh $1 "export ONEPANEL_MULTICAST_ADDRESS=$ONEPANEL_MULTICAST_ADDRESS ; rpm -Uvh $SETUP_DIR/$2 --nodeps --force ; sleep 5" || error "Cannot install $2 package on $1"
}

#####################################################################
# oneprovider functions
#####################################################################

# $1 - target host
function start_oneprovider {
    info "Starting oneprovider nodes..."
    
    if [[ "$ONEPROVIDER_DEVELOPER_MODE" == "yes" ]]; then
        oneprovider_nodes=`echo "$ONEPROVIDER_NODES" | tr ";" "\n"`
        for oneprovider_node in ${oneprovider_nodes}; do
            START_ERL=`ssh ${oneprovider_node} "cat /opt/oneprovider/files/oneprovider_node/releases/start_erl.data" 2>/dev/null`
            APP_VSN=${START_ERL#* }
            ssh ${oneprovider_node} "sed -i -e \"s/-oneprovider_node developer_mode.*/-oneprovider_node developer_mode true/g\" /opt/oneprovider/files/oneprovider_node/releases/$APP_VSN/vm.args" || error "Cannot enable developer mode on $oneprovider_node"
        done
    fi

    db_hosts=""
    db_nodes=`echo "$ONEPROVIDER_DB_NODES" | tr ";" "\n"`
    for db_node in ${db_nodes}; do
        db_hosts="\\\"`node_name "$db_node"`\\\",$db_hosts"
        ssh ${db_node} "sed -i -e \"s/^bind_address = [0-9\.]*/bind_address = 0.0.0.0/g\" /opt/oneprovider/files/database_node/etc/default.ini" || error "Cannot set oneprovider DB bind address on $db_node."
    done
    db_hosts=`echo "$db_hosts" | sed -e 's/.$//'`

    i=1
    ccm_hosts=""
    worker_hosts=""
    oneprovider_types=`echo "$ONEPROVIDER_TYPES" | tr ";" "\n"`
    for oneprovider_type in ${oneprovider_types}; do
        oneprovider_node=`nth "$ONEPROVIDER_NODES" ${i}`
        if [[ ${oneprovider_type} == "ccm_plus_worker" ]]; then
            ccm_hosts="\\\"`node_name "$oneprovider_node"`\\\",$ccm_hosts"
            worker_hosts="\\\"`node_name "$oneprovider_node"`\\\",$worker_hosts"
        else
            worker_hosts="\\\"`node_name "$oneprovider_node"`\\\",$worker_hosts"
        fi
        i=$(( i+1 ))
    done
    ccm_hosts=`echo "$ccm_hosts" | sed -e 's/.$//'`
    worker_hosts=`echo "$worker_hosts" | sed -e 's/.$//'`

    main_ccm_host=`echo "$ccm_hosts" | awk -F ',' '{print $1}' | xargs`

    storage_paths=""
    oneprovider_storage_paths=`echo "$ONEPROVIDER_STORAGE_PATHS" | tr ";" "\n"`
    for storage_path in ${oneprovider_storage_paths}; do
        storage_paths="\\\"$storage_path\\\",$storage_paths"
        # Add storage paths on all oneprovider nodes
        oneprovider_nodes=`echo "$ONEPROVIDER_NODES" | tr ";" "\n"`
        for oneprovider_node in ${oneprovider_nodes}; do
            ssh ${oneprovider_node} "mkdir -p $storage_path"
        done
    done
    storage_paths=`echo "$storage_paths" | sed -e 's/.$//'`

    ssh $1 "echo \"{\\\"Main CCM host\\\",  \\\"$main_ccm_host\\\"}.\" >  $SETUP_DIR/install.cfg"
    ssh $1 "echo \"{\\\"CCM hosts\\\",         [$ccm_hosts]}.\"        >> $SETUP_DIR/install.cfg"
    ssh $1 "echo \"{\\\"Worker hosts\\\",      [$worker_hosts]}.\"     >> $SETUP_DIR/install.cfg"
    ssh $1 "echo \"{\\\"Database hosts\\\",    [$db_hosts]}.\"         >> $SETUP_DIR/install.cfg"
    ssh $1 "echo \"{\\\"Storage paths\\\",     [$storage_paths]}.\"    >> $SETUP_DIR/install.cfg"

    ssh -tt -q $1 "onepanel_admin --install $SETUP_DIR/install.cfg 2>&1" 2>/dev/null || error "Cannot setup and start oneprovider nodes."
}

# $1 - target host
function remove_oneprovider {
    info "Removing oneprovider nodes..."

    local onepanel_admin
    onepanel_admin=$(ssh $1 "which onepanel_admin 2> /dev/null")
    if [[ $? == 0 ]]; then
        ssh -tt -q $1 "onepanel_admin --uninstall 2>&1" 2>/dev/null
    fi

    oneprovider_storage_paths=`echo "$ONEPROVIDER_STORAGE_PATHS" | tr ";" "\n"`
    for storage_path in ${oneprovider_storage_paths}; do
        ssh $1 "[ -z $storage_path ] || rm -rf $storage_path/users $storage_path/groups $storage_path/vfs_storage.info"
    done
    
    ssh $1 "rpm -e oneprovider 2> /dev/null"
    ssh $1 "rm -rf /opt/oneprovider" || error "Cannot remove oneprovider directory."
}

#####################################################################
# oneclient functions
#####################################################################

# $1 - target host
# $2 - target mountpoint
# $3 - peer certificate path
# $4 - target oneprovider's hostname
function install_oneclient {
    info "Moving oneclient files to $1..."

    RUID=`ssh $1 "echo \\\$UID"`
    S_DIR="${SETUP_DIR}_${RUID}"


    ssh $1 "mkdir -p $S_DIR" || error "Cannot create tmp setup dir '$S_DIR' on $1"
    ssh $1 "mkdir -p $2" || error "Cannot create mountpoint dir '$2' on $1"

    scp oneclient*.rpm $1:${S_DIR}/oneclient.rpm || error "Moving .rpm file failed"
    scp oneclient $1:~/oneclient || error "Moving oneclient binary file failed"
    scp $3 $1:${S_DIR}/peer.pem 2>/dev/null || error "Moving $3 file failed"
    ssh $1 "chmod 600 $S_DIR/peer.pem"
    ssh $1 "chmod +x ~/oneclient"

    info "Installing oneclient for user with UID: $RUID"

    if [[ "$RUID" == "0" ]]; then
        ssh $1 "yum install $S_DIR/oneclient.rpm -y" || error "Cannot install oneclient on $1"
    fi
}

# $1 - user id
# $2 - target host
# $3 - target mountpoint
# $4 - peer certificate path
# $5 - auth type
# $6 - oneclient node number
function start_oneclient {
    info "Starting oneclient on $2..."

    RUID=`ssh $2 "echo \\\$UID"`
    S_DIR="${SETUP_DIR}_${RUID}"
    provider_host_count=`len "$ONEPROVIDER_NODES"`
    provider_host_i=$(($6 % $provider_host_count))
    provider_host_i=$(($provider_host_i + 1))
    provider_host=`nth "$ONEPROVIDER_NODES" "$provider_host_i"`
    globalregistry_host=`nth "$GLOBALREGISTRY_NODES" 1`
    provider_host=${provider_host#*@}

    provider_hostname=""

    if [[ "$provider_host" != "" ]]; then
        provider_hostname="PROVIDER_HOSTNAME=\"$provider_host\""
    fi

    if [[ "$5" == "token" ]]; then
        ssh ${globalregistry_host} "mkdir -p ${SETUP_DIR}"
        scp globalregistry_setup.escript ${globalregistry_host}:${SETUP_DIR} || error "Cannot copy globalregistry setup escript to ${globalregistry_host}."
        escript_bin=`ssh ${globalregistry_host} "find /opt/globalregistry -name escript | head -1"` || error "Cannot find escript binary on ${globalregistry_host}"
        token=`ssh ${globalregistry_host} "${escript_bin} ${SETUP_DIR}/globalregistry_setup.escript --get_client_auth_code $1" 2>/dev/null` || error "Cannot get client auth code for user $1"

        ssh $2 "echo $token | $provider_hostname PATH=\$HOME:\$PATH oneclient --no-check-certificate --authentication token $3" || error "Cannot mount oneclient on $2 using token"
    else
        ssh $2 "$provider_hostname PEER_CERTIFICATE_FILE=\"$S_DIR/peer.pem\" PATH=\$HOME:\$PATH oneclient --no-check-certificate $3" || error "Cannot mount oneclient on $2 using certificate"
    fi
}

# $1 - target host
# $2 - target mountpoint
function remove_oneclient {
    info "Removing oneclient..."

    RUID=`ssh $1 "echo \\\$UID"`
    ssh $1 "killall -9 oneclient 2> /dev/null"
    ssh $1 "fusermount -u $2 2> /dev/null"
    ssh $1 "rm -rf $2"
    ssh $1 "rm -rf ${SETUP_DIR}_${RUID}"

    ssh $1 'rm -rf ~/oneclient'
    if [[ "$RUID" == "0" ]]; then
        ssh $1 'yum remove oneclient -y 2> /dev/null'
    fi
}

#####################################################################
# globalregistry functions
#####################################################################

# $1 - target host
function start_globalregistry {
    info "Starting globalregistry nodes..."

    db_hosts=""
    db_nodes=`echo "$GLOBALREGISTRY_DB_NODES" | tr ";" "\n"`
    for db_node in ${db_nodes}; do
        db_hosts="\\\"`node_name "$db_node"`\\\",$db_hosts"
        ssh ${db_node} "sed -i -e \"s/^bind_address = [0-9\.]*/bind_address = 0.0.0.0/\" /opt/globalregistry/files/database_node/etc/default.ini" || error "Cannot set globalregistry DB bind address on $db_node."
    done
    db_hosts=`echo "$db_hosts" | sed -e 's/.$//'`

    oneprovider_node=`nth "$GLOBALREGISTRY_NODES" 1`
    oneprovider_host=`node_name "$oneprovider_node"`

    ssh $1 "echo \"{\\\"Global Registry host\\\", \\\"$oneprovider_host\\\"}.\" >  $SETUP_DIR/install.cfg"
    ssh $1 "echo \"{\\\"Database hosts\\\",          [$db_hosts]}.\"         >> $SETUP_DIR/install.cfg"

    ssh -tt -q $1 "onepanel_admin --install $SETUP_DIR/install.cfg 2>&1" 2>/dev/null || error "Cannot setup and start globalregistry nodes."
}

# $1 - target host
function remove_globalregistry {
    info "Removing globalregistry nodes..."

    local onepanel_admin
    onepanel_admin=$(ssh $1 "which onepanel_admin 2> /dev/null")
    if [[ $? == 0 ]]; then
        ssh -tt -q $1 "onepanel_admin --uninstall 2>&1" 2>/dev/null
    fi

    ssh $1 "rpm -e globalregistry 2> /dev/null"
    rm -rf /opt/globalregistry || error "Cannot remove globalregistry directory."
}