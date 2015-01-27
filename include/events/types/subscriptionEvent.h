/**
* @file subscriptionEvent.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_EVENT_H
#define ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

class SubscriptionEventSerializer;

class SubscriptionEvent : public Event {
    friend class SubscriptionEventSerializer;

public:
    SubscriptionEvent(unsigned long long id);

    virtual ~SubscriptionEvent() = default;

    friend std::ostream &operator<<(std::ostream &,
                                    const SubscriptionEvent &event);

    virtual void emit() override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

private:
    unsigned long long m_id;
};

class SubscriptionEventSerializer : public EventSerializer {
public:
    virtual ~SubscriptionEventSerializer() = default;

    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif