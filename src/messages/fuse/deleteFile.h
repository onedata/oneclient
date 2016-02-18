/**
 * @file deleteFile.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_DELETE_FILE_H
#define ONECLIENT_MESSAGES_FUSE_DELETE_FILE_H

#include "messages/clientMessage.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The DeleteFile class represents a FUSE request for file deletion.
 */
class DeleteFile : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to be deleted.
     */
    DeleteFile(std::string uuid);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_DELETE_FILE_H
