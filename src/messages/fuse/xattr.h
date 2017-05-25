/**
 * @file xattr.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c XAttr represents a single extended attribute name and value
 * assigned to a file.
 */
class XAttr : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c Xattr response to a getxattr request.
     */
    XAttr(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return The extended attribute name.
     */
    const std::string &name() const;

    /**
     * @return The extended attribute value.
     */
    const std::string &value() const;

    std::string toString() const override;

private:
    std::string m_name;
    std::string m_value;
};

} // namespace fuse
} // namespace messages
} // namespace one
