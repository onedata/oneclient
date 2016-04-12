/**
 * @file fuseOperations.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fuseOperations.h"

namespace {
bool fuseEnabled = false;
} // namespace

namespace one {
namespace helpers {

struct fuse *fuseNew(struct fuse_chan *ch, struct fuse_args *args,
    const struct fuse_operations *op, size_t op_size, void *user_data)
{
    fuseEnabled = true;
    return fuse_new(ch, args, op, op_size, user_data);
}

bool fuseInterrupted() { return fuseEnabled && fuse_interrupted(); }

} // namespace helpers
} // namespace one