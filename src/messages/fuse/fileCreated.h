/**
 * @file fileCreated.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_CREATED_H
#define ONECLIENT_MESSAGES_FUSE_FILE_CREATED_H

#include "fuseResponse.h"

namespace one {
namespace messages {
namespace fuse {

class FileAttr;
class FileLocation;

/**
 * The @c FileCreated class represents a server response for successful file
 * creation.
 */
class FileCreated : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileCreated counterpart
     */
    FileCreated(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A handleID representing the handle on the server.
     */
    const std::string &handleId() const;

    /**
     * @return Attributes of the file.
     */
    const FileAttr &attr() const;

    /**
     * @return Location of the file.
     */
    const FileLocation &location() const;

    std::string toString() const override;

private:
    std::string m_handleId;
    std::unique_ptr<FileAttr> m_attr;
    std::unique_ptr<FileLocation> m_location;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_CREATED_H
