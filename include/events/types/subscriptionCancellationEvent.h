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

/**
* The SubscriptionCancellationEvent class represents a successful subscription
* cancellation.
*/
class SubscriptionCancellationEvent : public Event {
    friend class SubscriptionCancellationEventSerializer;
    friend std::ostream &operator<<(std::ostream &,
                                    const SubscriptionCancellationEvent &event);

public:
    /**
    * Constructor.
    * @param id ID of subscription to be cancelled.
    */
    SubscriptionCancellationEvent(unsigned long long id);

    virtual ~SubscriptionCancellationEvent() = default;

    /**
    * Does noting.
    * A @c SubscriptionCancellationEvent can not be emitted by itself.
    */
    virtual void emit() override;

    /**
    * @copydoc Event::serializer()
    */
    virtual std::unique_ptr<EventSerializer> serializer() const override;

private:
    unsigned long long m_id;
};

/**
* The SubscriptionCancellationEventSerializer class is responsible for
* serialization of the @c SubscriptionCancellationEvent objects.
*/
class SubscriptionCancellationEventSerializer : public EventSerializer {
public:
    virtual ~SubscriptionCancellationEventSerializer() = default;

    /**
    * @copydoc EventSerializer::serialize(unsigned long long, const Event &)
    */
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_CANCELLATION_EVENT_H