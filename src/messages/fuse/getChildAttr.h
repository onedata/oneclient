/**
 * @file getChildAttr.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fileRequest.h"

#include <boost/filesystem/path.hpp>
#include <folly/FBString.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * @c GetChildAttr represents a FUSE request for file attributes of a
 * directory's child.
 */
class GetChildAttr : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid Uuid of a parent directory.
     * @param name Name of parent's child to look up.
     */
    GetChildAttr(folly::fbstring uuid, folly::fbstring name);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring n_uuid;
    folly::fbstring m_name;
};

} // namespace fuse
} // namespace messages
} // namespace one
