/**
 * @file storageTestFileVerification.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_VERIFICATION_H
#define ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_VERIFICATION_H

#include "messages/serverMessage.h"
#include "helperParams.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class StorageTestFileVerification;
}
namespace messages {
namespace fuse {

/**
* The StorageTestFile class represents a message that is sent by the server to
* provide status of storage test file verification, that is whether the test
* file has been successfully modified by the client.
*/
class StorageTestFileVerification : public ServerMessage {
public:
    using ProtocolMessage = clproto::StorageTestFileVerification;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * StorageTestFile counterpart.
     */
    StorageTestFileVerification(const ProtocolMessage &message);

    /**
     * @return ID of a storage on which test file is located.
     */
    const std::string &storageId() const;

    std::string toString() const override;

private:
    std::string m_storageId;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_VERIFICATION_H
