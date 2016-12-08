/**
 * @file fileRenamed.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_RENAMED_H
#define ONECLIENT_EVENTS_TYPES_FILE_RENAMED_H

#include "messages/fuse/fileRenamedEntry.h"
#include "singleEvent.h"

#include <vector>

namespace one {
namespace clproto {
class FileRenamedEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FileRenamed class represents a rename file operation in the system.
 */
class FileRenamed : public SingleEvent {
    using ProtocolMessage = clproto::FileRenamedEvent;
    using FileRenamedEntry = messages::fuse::FileRenamedEntry;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c FileRenamed
     * counterpart.
     */
    FileRenamed(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * @return An entry describing changes in the renamed file.
     */
    const FileRenamedEntry &topEntry() const;

    /**
     * @return List of entries describing changes in children of the renamed
     * file.
     */
    const std::vector<FileRenamedEntry> &childEntries() const;

    std::string toString() const override;

private:
    FileRenamedEntry m_topEntry;
    std::vector<FileRenamedEntry> m_childEntries;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_RENAMED_H
