/**
* @file event.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include <cstddef>

namespace one {
namespace client {
namespace events {

/**
* The Event class represents an operation in the file system.
*/
class Event {
public:
    virtual ~Event() = default;

protected:
    std::size_t m_counter = 1;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H