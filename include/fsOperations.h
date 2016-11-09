/**
 * @file fsOperations.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_OPERATIONS_H
#define ONECLIENT_FS_OPERATIONS_H

#include <fuse/fuse_lowlevel.h>

struct fuse_lowlevel_ops fuseOperations();

#endif // ONECLIENT_FS_OPERATIONS_H
