#!/usr/bin/env bash

V=`git describe --tags`
V_ARR=(${V//./ })
V_MAJ=${V_ARR[0]}
V_MIN=${V_ARR[1]}
S=${V_ARR[2]}
S_ARR=(${S//-/ })
V_PAT=${S_ARR[0]}
echo "set( SAVED_VERSION_MAJOR $V_MAJ)
set( SAVED_VERSION_MINOR $V_MIN)
set( SAVED_VERSION_PATCH $V_PAT)" > $1/version.txt
