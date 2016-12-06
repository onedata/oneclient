/**
 * @file fileLocationChanged.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class FileLocationChangedEvent;
} // namespace clproto
namespace client {
namespace events {

class FileLocationChanged : public Event {
    using ProtocolMessage = clproto::FileLocationChangedEvent;

public:
    FileLocationChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    const std::string &aggregationKey() const override;

    const FileLocation &fileLocation() const;

    std::string toString() const override;

    void aggregate(EventPtr<FileLocationChanged> event);

private:
    std::unique_ptr<FileLocation> m_fileLocation;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H
