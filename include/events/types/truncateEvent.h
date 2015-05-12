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
     * @param fileId ID of file associated with a truncate operation.
     * @param fileSize Size of file after a truncate operation.
     */
    TruncateEvent(std::string fileId, off_t fileSize);

    virtual std::string toString() const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H
