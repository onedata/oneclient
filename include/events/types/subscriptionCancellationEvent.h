/**
* @file subscriptionCancellationEvent.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_CANCELLATION_EVENT_H
#define ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_CANCELLATION_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

class SubscriptionCancellationEventSerializer;

class SubscriptionCancellationEvent : public Event {
    friend class SubscriptionCancellationEventSerializer;

public:
    SubscriptionCancellationEvent(unsigned long long id);

    virtual ~SubscriptionCancellationEvent() = default;

    friend std::ostream &operator<<(std::ostream &,
                                    const SubscriptionCancellationEvent &event);

    virtual void emit() override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

private:
    unsigned long long m_id;
};

class SubscriptionCancellationEventSerializer : public EventSerializer {
public:
    virtual ~SubscriptionCancellationEventSerializer() = default;

    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_CANCELLATION_EVENT_H