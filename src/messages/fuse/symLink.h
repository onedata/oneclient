/**
 * @file symLink.h
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c SymLink represents the response to a ReadSymLink request.
 */
class SymLink : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c SymLink response to a readlink request.
     */
    SymLink(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return The symbolic link value.
     */
    const std::string &link() const;

    std::string toString() const override;

private:
    std::string m_link;
};

} // namespace fuse
} // namespace messages
} // namespace one
