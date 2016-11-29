/**
 * @file truncateEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H

#include "writeEvent.h"

namespace one {
namespace client {
namespace events {

class TruncateEvent : public WriteEvent {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param fileSize Size of a file after write operation.
     * occurred.
     */
    TruncateEvent(std::string fileUuid, off_t fileSize)
        : WriteEvent(std::move(fileUuid), 0, 0, "", "", fileSize)
    {
    }
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_TRUNCATE_EVENT_H
