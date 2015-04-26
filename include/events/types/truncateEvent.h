/**
* @file truncateEvent.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H

#include "writeEvent.h"

namespace one {
namespace client {
namespace events {

template <class EventType> class EventStream;

/**
* The TruncateEvent class represents a truncate operation in the file system.
*/
class TruncateEvent : public WriteEvent {
public:
    /**
    * Constructor.
    * @param eventStream Weak pointer to @c WriteEventStream to which this event
    * will be pushed when emitted.
    * @param fileId ID of file associated with a truncate operation.
    * @param fileSize Size of file after a truncate operation.
    */
    TruncateEvent(std::weak_ptr<EventStream<WriteEvent>> eventStream,
        std::string fileId, off_t fileSize)
        : WriteEvent(
              std::move(eventStream), std::move(fileId), 0, 0, fileSize){};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H