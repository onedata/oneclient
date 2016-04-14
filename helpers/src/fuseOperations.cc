/**
 * @file fuseOperations.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fuseOperations.h"

namespace {
thread_local bool fuseSessionActive = false;
} // namespace

namespace one {
namespace helpers {

void activateFuseSession() { fuseSessionActive = true; }

bool fuseInterrupted() { return fuseSessionActive && fuse_interrupted(); }

} // namespace helpers
} // namespace one