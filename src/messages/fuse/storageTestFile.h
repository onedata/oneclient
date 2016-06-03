/**
 * @file storageTestFile.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_H
#define ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_H

#include "helperParams.h"
#include "messages/serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class StorageTestFile;
}
namespace messages {
namespace fuse {

/**
* The StorageTestFile class represents a message that is sent by the server to
* provide location of a created storage test file along with helper arguments
* used to access the file.
*/
class StorageTestFile : public ServerMessage {
public:
    using ProtocolMessage = clproto::StorageTestFile;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c ServerMessage.
     */
    StorageTestFile(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Storage helper parameters used to access test file.
     */
    const HelperParams &helperParams() const;

    /**
     * @return UUID of a space in which test file is located.
     */
    const std::string &spaceUuid() const;

    /**
     * @return Storage ID of a test file.
     */
    const std::string &fileId() const;

    /**
     * @return Storage test file content.
     */
    const std::string &fileContent() const;

    std::string toString() const override;

private:
    HelperParams m_helperParams;
    std::string m_spaceUuid;
    std::string m_fileId;
    std::string m_fileContent;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_STORAGE_TEST_FILE_H
