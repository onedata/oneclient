/**
* @file eventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_SUBSCRIPTION_H

#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class EventStream;
class EventBuffer;

class EventSubscription {
public:
    virtual ~EventSubscription() = default;

    virtual std::unique_ptr<EventStream>
    createEventStream(const EventSubscription &subscription,
                      std::weak_ptr<Context> context,
                      std::weak_ptr<EventBuffer> buffer) const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif