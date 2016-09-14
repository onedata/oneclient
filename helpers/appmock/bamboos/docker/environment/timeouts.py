# coding=utf-8
"""Authors: Jakub Kudzia
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

This file contains timeouts definitions for scripts that bring up test environment
"""

MINUTE = 60
APPMOCK_WAIT_FOR_NAGIOS_SECONDS = 5 * MINUTE
CEPH_READY_WAIT_SECONDS = 5 * MINUTE
SWIFT_READY_WAIT_SECONDS = 5 * MINUTE
COUCHBASE_READY_WAIT_SECONDS = 5 * MINUTE
DNS_WAIT_SECONDS = 5 * MINUTE
RIAK_READY_WAIT_SECONDS = 5 * MINUTE
CLUSTER_WAIT_FOR_NAGIOS_SECONDS = 5 * MINUTE
REQUEST_TIMEOUT = 20
