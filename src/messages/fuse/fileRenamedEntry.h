/**
 * @file fileRenamedEntry.h
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_ENTRY_H
#define ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_ENTRY_H

#include <memory>
#include <string>

namespace one {
namespace clproto {
class FileRenamedEntry;
}
namespace messages {
namespace fuse {

/**
 * @c FileRenamedEntry provides information about file metadata changed after
 * renaming
 */
class FileRenamedEntry {
public:
    using ProtocolMessage = clproto::FileRenamedEntry;
    FileRenamedEntry() = default;

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c FileRenamedEntry
     * counterpart.
     */
    FileRenamedEntry(const ProtocolMessage &message);

    /**
     * @return Old Uuid of renamed file.
     */
    const std::string &oldUuid() const;

    /**
     * @return New Uuid of renamed file.
     */
    const std::string &newUuid() const;

    /**
     * @return New path of renamed file.
     */
    const std::string &newPath() const;

    std::string toString() const;

    void fillProtocolMessage(ProtocolMessage &message);

private:
    std::string m_oldUuid;
    std::string m_newUuid;
    std::string m_newPath;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_ENTRY_H
