/**
 * @file fileRenamed.h
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_H
#define ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_H

#include "fileRenamedEntry.h"
#include "fuseResponse.h"

namespace one {
namespace clproto {
class FileRenamed;
}
namespace messages {
namespace fuse {

/**
 * The @c FileRenamed class represents server-sent information about file
 * renaming.
 */
class FileRenamed : public FuseResponse {
public:
    using ProtocolMessage = clproto::FileRenamed;

    FileRenamed() = default;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileRenamed counterpart
     */
    FileRenamed(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return New UUID of renamed file.
     */
    const std::string &newUuid() const;

    /**
     * @return List of entries describing changes in children of renamed file.
     */
    const std::vector<FileRenamedEntry> &childEntries() const;

    std::string toString() const override;

private:
    std::string m_newUuid;
    std::vector<FileRenamedEntry> m_childEntries;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_H
