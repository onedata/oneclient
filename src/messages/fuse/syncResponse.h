/**
 * @file syncResponse.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CHECKSUM_H
#define ONECLIENT_MESSAGES_FUSE_CHECKSUM_H

#include "fileLocation.h"
#include "fuseResponse.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class SyncResponse;
}
namespace messages {
namespace fuse {

/**
 * The SyncResponse class represents a message with md5 sum of synced data that
 * is sent by the server.
 */
class SyncResponse : public FuseResponse {
public:
    using ProtocolMessage = clproto::SyncResponse;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c ServerMessage.
     */
    SyncResponse(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Checksum value.
     */
    const std::string &checksum() const { return m_checksum; }

    /**
     * @return File location info.
     */
    const FileLocation &fileLocation() const { return m_fileLocation; }

    std::string toString() const override;

private:
    std::string m_checksum;
    FileLocation m_fileLocation;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CHECKSUM_H
