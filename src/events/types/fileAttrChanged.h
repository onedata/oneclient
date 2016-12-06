/**
 * @file fileAttrChanged.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class FileAttrChangedEvent;
} // namespace clproto
namespace client {
namespace events {

class FileAttrChanged : public Event {
    using ProtocolMessage = clproto::FileAttrChangedEvent;

public:
    FileAttrChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    const std::string &aggregationKey() const override;

    const FileAttr &fileAttr() const;

    std::string toString() const override;

    void aggregate(EventPtr<FileAttrChanged> event);

private:
    std::unique_ptr<FileAttr> m_fileAttr;
    std::string m_aggregationKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H
