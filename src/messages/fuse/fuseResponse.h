/**
 * @file fuseResponse.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FUSE_RESPONSE_H
#define ONECLIENT_MESSAGES_FUSE_FUSE_RESPONSE_H

#include "messages/serverMessage.h"

#include <memory>
#include <string>
#include <vector>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FuseResponse class represents a response to a fuse request.
 */
class FuseResponse : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FuseResponse counterpart.
     * @note The constructor throws an applicable std::system_error exception if
     * received message's status is not OK.
     */
    FuseResponse(const std::unique_ptr<ProtocolServerMessage> &serverMessage);

    virtual std::string toString() const override;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FUSE_RESPONSE_H
