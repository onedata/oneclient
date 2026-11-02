/**
 * @file composite.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fsLogic.h"
#include "inFiber.h"
#include "withUuids.h"

namespace one {
namespace client {
namespace fslogic {

using Composite = InFiber<WithUuids<FsLogic>>;

#if defined(CLANG_UML_TRAVERSE)
struct CompositeWrapper {
    Composite composite;
};
#endif

} // namespace fslogic
} // namespace client
} // namespace one
