#!/usr/bin/env bash

# samba
mkdir -p /run/samba
smbd
nmbd

# nfs
rpcbind
rpc.nfsd
exportfs -ar
rpc.mountd
rpc.nfsd
rpc.statd

oneclient -f "$@"
