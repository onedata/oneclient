/**
 * @file deleteFile.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_DELETE_FILE_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_DELETE_FILE_H

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

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_DELETE_FILE_H
