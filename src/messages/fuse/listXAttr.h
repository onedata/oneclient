/**
 * @file listXAttr.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fileRequest.h"

#include <folly/FBString.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The ListXAttr class represents a provider request for file's extended
 * attribute list.
 */
class ListXAttr : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which extended attributes are requested.
     */
    ListXAttr(folly::fbstring uuid);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;
};

} // namespace fuse
} // namespace messages
} // namespace one