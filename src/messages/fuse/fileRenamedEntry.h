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
     * Constructor.
     * @param message Protocol Buffers message representing @c FileRenamedEntry
     * counterpart.
     */
    FileRenamedEntry(ProtocolMessage &message);

    /**
     * @return Old Uuid of renamed file.
     */
    const std::string &oldUuid() const { return m_oldUuid; }

    /**
     * @return New Uuid of renamed file.
     */
    const std::string &newUuid() const { return m_newUuid; }

    /**
     * @return New uuid of renamed file's parent.
     */
    const std::string &newParentUuid() const { return m_newParentUuid; }

    /**
     * @return New name of renamed file.
     */
    const std::string &newName() const { return m_newName; }

    std::string toString() const;

    void fillProtocolMessage(ProtocolMessage &message);

private:
    std::string m_oldUuid;
    std::string m_newUuid;
    std::string m_newParentUuid;
    std::string m_newName;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_RENAMED_ENTRY_H
