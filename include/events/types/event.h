/**
* @file event.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include "messages/clientMessage.h"

#include <cstddef>

namespace one {
namespace client {
namespace events {

/**
* The Event class represents an operation in the file system.
*/
class Event : public one::messages::ClientMessage {
public:
    virtual ~Event() = default;

    /**
    * @return Event's counter.
    */
    std::size_t counter() const { return m_counter; }

protected:
    std::size_t m_counter = 1;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H
