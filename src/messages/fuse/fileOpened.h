/**
 * @file fileOpened.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_OPENED_H
#define ONECLIENT_MESSAGES_FUSE_FILE_OPENED_H

#include "fuseResponse.h"

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c FileOpened class represents a server response for successful file
 * opening.
 */
class FileOpened : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileOpened counterpart
     */
    FileOpened(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
    * @return A handleID representing the handle on the server.
    */
    const std::string &handleId() const;

    std::string toString() const override;

private:
    std::string m_handleId;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_OPENED_H
