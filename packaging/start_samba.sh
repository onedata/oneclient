#!/usr/bin/env bash
if [[ -v MOUNT_PATH ]]; then
    echo "[global]
guest account = root
[onedata]
path = $MOUNT_PATH
read only = no
guest ok = yes
public = yes
writable = yes" > /etc/samba/smb.conf
    mkdir -p $MOUNT_PATH
    mkdir -p /run/samba
    smbd
    nmbd
fi
