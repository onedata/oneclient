/**
* @file eventFactory.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_FACTORY_H
#define ONECLIENT_EVENTS_EVENT_FACTORY_H

#include <memory>

namespace one {
namespace client {
namespace events {

class Event;

class EventFactory {
public:
    std::unique_ptr<Event> createReadEvent(const std::string &fileId,
                                           off_t offset, size_t size) const;

    std::unique_ptr<Event> createWriteEvent(const std::string &fileId,
                                            off_t offset, size_t size,
                                            off_t fileSize) const;

    std::unique_ptr<Event> createTruncateEvent(const std::string &fileId,
                                               off_t fileSize) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif