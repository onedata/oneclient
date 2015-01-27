/**
* @file eventMapper.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_MAPPER_H
#define ONECLIENT_EVENTS_EVENT_MAPPER_H

#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class Event;
class EventBuffer;
class EventStream;

class EventMapper {
public:
    EventMapper(std::shared_ptr<Context> context);

    void map(const Event &event);

    const std::string &addOrUpdateEventStream(const EventStream &stream);

    bool removeEventStream(const std::string &id);

private:
    const std::shared_ptr<Context> m_context;
};

} // namespace events
} // namespace client
} // namespace one

#endif