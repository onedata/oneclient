/**
 * @file getFSStats.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "messages/clientMessage.h"

#include <memory>
#include <sstream>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c GetFSStats class represents FUSE request for getting filesystem
 * stats. Filesystem stats are separate for each Onedata space.
 */
class GetFSStats : public ClientMessage {
public:
    /**
     * Constructor.
     * @param fileId Guid of space or file within a space.
     */
    GetFSStats(std::string fileId);

    virtual ~GetFSStats() = default;

    std::string toString() const override;

protected:
    virtual std::unique_ptr<ProtocolClientMessage>
    serializeAndDestroy() override;

    std::string m_fileId;
};

} // namespace fuse
} // namespace messages
} // namespace one
