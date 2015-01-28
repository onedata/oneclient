/**
* @file writeEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H

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
class WriteEventStream;

/**
* Name of a @c WriteEventSubscription message.
*/
static const std::string WRITE_EVENT_SUBSCRIPTION_MESSAGE =
    one::clproto::events::WriteEventSubscription::descriptor()->name();

/**
* The WriteEventSubscription class represents a message sent by the server to
* subscribe for write events.
*/
class WriteEventSubscription {
    friend class WriteEventStream;
    friend std::ostream &operator<<(std::ostream &,
                                    const WriteEventSubscription &subscription);

public:
    /**
    * Constructor.
    * @param id Subscription id.
    */
    WriteEventSubscription(unsigned long long id);

    /**
    * Sets size threshold of subscription.
    * @param sizeThreshold Maximal amount of write bytes before event emission.
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
    * Processes an write event subscription message by subscribing for write
    * events.
    * @param stream A @c WriteEventStream instance.
    * @param factory An @c EventFactory instance.
    * @param buffer An @c EventBuffer instance.
    */
    void process(std::weak_ptr<WriteEventStream> stream,
                 std::weak_ptr<EventFactory> factory,
                 std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
    boost::optional<size_t> m_sizeThreshold{};
    boost::optional<size_t> m_counterThreshold{};
    boost::optional<std::chrono::milliseconds> m_timeThreshold{};
};

/**
* The WriteEventSubscriptionSerializer class is responsible for deserialization
* of the @c WriteEventSubscription messages.
*/
class WriteEventSubscriptionSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Deserializes the @c WriteEventSubscription message.
    * @param message Message to deserialize.
    * @return Returns deserialized @c WriteEventSubscription instance.
    */
    std::unique_ptr<WriteEventSubscription>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H