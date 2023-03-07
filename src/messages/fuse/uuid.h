/**
 * @file uuid.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c Uuid represents a file Guid in Onedata.
 */
class Uuid : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * guid of a file or directory in Onedata.
     */
    Uuid(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return The uuid value.
     */
    const std::string &uuid() const;

    std::string toString() const override;

private:
    std::string m_uuid;
};

} // namespace fuse
} // namespace messages
} // namespace one
