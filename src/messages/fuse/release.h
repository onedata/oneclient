/**
 * @file release.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_RELEASE_H
#define ONECLIENT_MESSAGES_FUSE_RELEASE_H

#include "messages/clientMessage.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c Release class represents a FUSE request for file closure.
 */
class Release : public ClientMessage {
public:
    /**
     * Constructor.
     * @param handleId Id of the handle to release.
     */
    Release(std::string uuid, std::string handleId);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
    std::string m_handleId;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_RELEASE_H
