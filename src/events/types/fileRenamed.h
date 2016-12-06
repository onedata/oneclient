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

class FileRenamed : public SingleEvent {
    using ProtocolMessage = clproto::FileRenamedEvent;
    using FileRenamedEntry = messages::fuse::FileRenamedEntry;

public:
    FileRenamed(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    const FileRenamedEntry &topEntry() const;

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
