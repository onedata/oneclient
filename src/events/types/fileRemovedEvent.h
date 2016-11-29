/**
 * @file fileRemovedEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class FileRemovedEvent;
} // namespace clproto
namespace client {
namespace events {

class FileRemovedEvent : public Event {
    using ProtocolMessage = clproto::FileRemovedEvent;

public:
    FileRemovedEvent(const ProtocolMessage &msg);

    const std::string &routingKey() const override;

    const std::string &aggregationKey() const override;

    const std::string &fileUuid() const;

    std::string toString() const override;

    void aggregate(ConstEventPtr event) override;

    EventPtr clone() const override;

private:
    std::string m_fileUuid;
    std::string m_routingKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H
