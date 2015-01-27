/**
* @file eventMapper.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_MAPPER_H
#define ONECLIENT_EVENTS_EVENT_MAPPER_H

#include "events/types/event.h"

#include <map>
#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class EventBuffer;

class EventMapper {
public:
    EventMapper(std::shared_ptr<Context> context);

    void map(const Event &event);

    const std::string &
    addOrUpdateEventStream(std::unique_ptr<EventStream> stream);

    bool removeEventStream(const std::string &id);

private:
    std::shared_ptr<Context> m_context;
    std::map<Event::Type, std::unique_ptr<EventStream>> m_streams;
};

} // namespace events
} // namespace client
} // namespace one

#endif