/**
 * @file fuseOperations.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <fuse.h>

namespace one {
namespace helpers {

/**
 * Wraps the fuse_new function and sets @c fuseEnabled to true.
 * @return the created FUSE handle
 */
struct fuse *fuseNew(struct fuse_chan *ch, struct fuse_args *args,
    const struct fuse_operations *op, size_t op_size, void *user_data);

/**
 * Wraps the fuse_interrupted function.
 * @return true if @c fuseEnabled is set to true and FUSE operation has been
 * aborted by user, otherwise false.
 */
bool fuseInterrupted();

} // namespace helpers
} // namespace one