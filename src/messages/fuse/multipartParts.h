/**
 * @file multipartParts.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

#include "multipartPart.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Optional.h>

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FileChildren class represents server-sent file children list.
 */
class MultipartParts : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c MultipartParts counterpart.
     */
    MultipartParts(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A list of directory's children, specified by their UUID and
     * filename.
     */
    const folly::fbvector<MultipartPart> &parts() const;

    const bool isLast() const;

    folly::Optional<size_t> nextPartMarker() const;

    std::string toString() const override;

private:
    folly::fbvector<MultipartPart> m_parts;
    bool m_isLast{false};
    folly::Optional<size_t> m_nextPartMarker;
};

} // namespace fuse
} // namespace messages
} // namespace one
