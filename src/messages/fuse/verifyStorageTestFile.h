/**
 * @file verifyStorageTestFile.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_VERIFY_STORAGE_TEST_FILE_H
#define ONECLIENT_MESSAGES_FUSE_VERIFY_STORAGE_TEST_FILE_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The VerifyStorageTestFile class represents a message that is sent by the
 * client to request verification of a successful modification of the storage
 * test file.
 */
class VerifyStorageTestFile : public ClientMessage {
public:
    /**
     * Constructor.
     * @param storageId ID of a storage on which test file is located.
     * @param spaceId ID of a space in which test file is located.
     * @param fileId Storage ID of a test file.
     * @param fileContent Content of a test file.
     */
    VerifyStorageTestFile(std::string storageId, std::string spaceId,
        std::string fileId, std::string fileContent);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_storageId;
    std::string m_spaceId;
    std::string m_fileId;
    std::string m_fileContent;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_VERIFY_STORAGE_TEST_FILE_H
