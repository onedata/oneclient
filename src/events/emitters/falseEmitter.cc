/**
 * @file falseEmitter.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "timedEmitter.h"

namespace one {
namespace client {
namespace events {

void FalseEmitter::process(ConstEventPtr event) {}

bool FalseEmitter::ready() { return false; }

void FalseEmitter::reset() {}

} // namespace events
} // namespace client
} // namespace one
