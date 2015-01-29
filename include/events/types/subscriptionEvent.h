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

/**
* The SubscriptionCancellationEvent class represents a successful subscription.
*/
class SubscriptionEvent : public Event {
    friend class SubscriptionEventSerializer;
    friend std::ostream &operator<<(std::ostream &,
                                    const SubscriptionEvent &event);

public:
    /**
    * Constructor.
    * @param id ID of added subscription.
    */
    SubscriptionEvent(unsigned long long id);

    virtual ~SubscriptionEvent() = default;

    /**
    * Does noting.
    * A @c SubscriptionEvent can not be emitted by itself.
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
* The SubscriptionEventSerializer class is responsible for serialization of the
* @c SubscriptionEvent objects.
*/
class SubscriptionEventSerializer : public EventSerializer {
public:
    virtual ~SubscriptionEventSerializer() = default;

    /**
    * @copydoc EventSerializer::serialize(unsigned long long, const Event &)
    */
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long seqNum, const Event &event) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SUBSCRIPTION_EVENT_H