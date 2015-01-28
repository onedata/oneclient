/**
* @file readEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_READ_EVENT_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_MESSAGES_READ_EVENT_SUBSCRIPTION_H

#include "events.pb.h"

#include <chrono>
#include <boost/optional.hpp>

namespace one {

namespace clproto {
namespace communication_protocol {
class Answer;
}
}

namespace client {
namespace events {

class EventBuffer;
class EventFactory;
class ReadEventStream;

/**
* Name of a read event subscription message.
*/
static const std::string READ_EVENT_SUBSCRIPTION_MESSAGE =
    one::clproto::events::ReadEventSubscription::descriptor()->name();

/**
  * The ReadEventSubscription class represents a message sent by the server to
  * subscribe for read events.
  */
class ReadEventSubscription {
    friend class ReadEventStream;
    friend std::ostream &operator<<(std::ostream &,
                                    const ReadEventSubscription &subscription);

public:
    /**
    * Constructor.
    * @param id Subscription id.
    */
    ReadEventSubscription(unsigned long long id);

    /**
    * Sets size threshold of subscription.
    * @param sizeThreshold Maximal amount of read bytes before event emission.
    */
    void setSizeThreshold(size_t sizeThreshold);

    /**
    * Sets counter threshold of subscription.
    * @param counterThreshold Maximal amount of aggregated events before
    * emission.
    */
    void setCounterThreshold(size_t counterThreshold);

    /**
    * Sets time threshold of subscription.
    * @param timeThreshold Maximal delay in milliseconds between successive
    * events emission.
    */
    void setTimeThreshold(const std::chrono::milliseconds &timeThreshold);

    /**
    * Processes an read event subscription message by subscribing for read
    * events.
    * @param stream Weak pointer to a read event stream.
    * @param factory Weak pointer to an event factory.
    * @param buffer Weak pointer to an event buffer.
    */
    void process(std::weak_ptr<ReadEventStream> stream,
                 std::weak_ptr<EventFactory> factory,
                 std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
    boost::optional<size_t> m_sizeThreshold{};
    boost::optional<size_t> m_counterThreshold{};
    boost::optional<std::chrono::milliseconds> m_timeThreshold{};
};

/**
* The ReadEventSubscriptionSerializer class is responsible for deserialization
* of the ReadEventSubscription messages.
*/
class ReadEventSubscriptionSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Deserializes the ReadEventSubscription message.
    * @param message Message to deserialize.
    */
    std::unique_ptr<ReadEventSubscription>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_READ_EVENT_SUBSCRIPTION_H