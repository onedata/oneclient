/**
 * @file fsOperations.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_OPERATIONS_H
#define ONECLIENT_FS_OPERATIONS_H

#if FUSE_USE_VERSION > 30
#include <fuse3/fuse_lowlevel.h>
#else
#include <fuse/fuse_lowlevel.h>
#endif

struct fuse_lowlevel_ops fuseOperations();

#endif // ONECLIENT_FS_OPERATIONS_H
