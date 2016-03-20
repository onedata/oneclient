/**
 * @file createStorageTestFile.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CREATE_STORAGE_TEST_FILE_H
#define ONECLIENT_MESSAGES_FUSE_CREATE_STORAGE_TEST_FILE_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreateStorageTestFile class represents a message that is sent by the
 * client to request creation of a storage test file, which will be used during
 * procedure of directly accessible storages detection.
 */
class CreateStorageTestFile : public ClientMessage {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file for which storage helper has been
     * retrieved, in the directory of this file an actual storage test file will
     * be created.
     * @param storageId ID of a storage on which the test file will be created.
     */
    CreateStorageTestFile(std::string fileUuid, std::string storageId);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_fileUuid;
    std::string m_storageId;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CREATE_STORAGE_TEST_FILE_H
