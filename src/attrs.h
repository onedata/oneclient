/**
 * @file attrs.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <memory>

namespace one {

namespace messages {
namespace fuse {
class FileAttr;
class FileLocation;
} // namespace fuse
} // namespace messages

namespace client {

using FileAttr = messages::fuse::FileAttr;
using FileAttrPtr = std::shared_ptr<const FileAttr>;
using FileLocation = messages::fuse::FileLocation;
using FileLocationPtr = std::shared_ptr<const FileLocation>;

} // namespace client
} // namespace one
