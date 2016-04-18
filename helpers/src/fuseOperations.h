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
 * Defines FUSE session as active for calling thread.
 */
void activateFuseSession();

/**
 * Wraps the fuse_interrupted function.
 * @return true if @c fuseEnabled is set to true and FUSE operation has been
 * aborted by user, otherwise false.
 */
bool fuseInterrupted();

} // namespace helpers
} // namespace one