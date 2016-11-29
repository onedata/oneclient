/**
 * @file fileRenamedEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H

#include "event.h"
#include "messages/fuse/fileRenamedEntry.h"

#include <vector>

namespace one {
namespace clproto {
class FileRenamedEvent;
} // namespace clproto
namespace client {
namespace events {

class FileRenamedEvent : public Event {
    using ProtocolMessage = clproto::FileRenamedEvent;
    using FileRenamedEntry = messages::fuse::FileRenamedEntry;

public:
    FileRenamedEvent(const ProtocolMessage &msg);

    const std::string &routingKey() const override;

    const std::string &aggregationKey() const override;

    const FileRenamedEntry &topEntry() const;

    const std::vector<FileRenamedEntry> &childEntries() const;

    std::string toString() const override;

    void aggregate(ConstEventPtr event) override;

    EventPtr clone() const override;

private:
    FileRenamedEntry m_topEntry;
    std::vector<FileRenamedEntry> m_childEntries;
    std::string m_routingKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H
