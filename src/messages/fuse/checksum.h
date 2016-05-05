/**
 * @file checksum.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CHECKSUM_H
#define ONECLIENT_MESSAGES_FUSE_CHECKSUM_H

#include "messages/serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class Checksum;
}
namespace messages {
namespace fuse {

/**
 * The Checksum class represents a message with md5 sum of synced data that is
 * sent by the server.
 */
class Checksum : public ServerMessage {
public:
    using ProtocolMessage = clproto::Checksum;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c ServerMessage.
     */
    Checksum(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Checksum value.
     */
    const std::string &value() const;

    std::string toString() const override;

private:
    std::string m_value;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CHECKSUM_H
