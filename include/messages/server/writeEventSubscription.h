/**
* @file writeEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_SERVER_WRITE_EVENT_SUBSCRIPTION_H
#define ONECLIENT_MESSAGES_SERVER_WRITE_EVENT_SUBSCRIPTION_H

#include "serverMessage.h"

#include <chrono>
#include <memory>

namespace one {
namespace client {

namespace events {
class WriteEvent;
template <class EventType, class SubscriptionType> class EventStream;
}

/**
* The WriteEventSubscription class represents write event subscription request
* sent by the server.
*/
class WriteEventSubscription : public ServerMessage {
    friend class events::EventStream<events::WriteEvent,
                                     WriteEventSubscription>;

public:
    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * WriteEventSubscription counterpart.
    */
    WriteEventSubscription(
        std::unique_ptr<ProtocolServerMessage> serverMessage);

private:
    uint64_t m_id;
    size_t m_counterThreshold;
    std::chrono::milliseconds m_timeThreshold;
    size_t m_sizeThreshold;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_SERVER_WRITE_EVENT_SUBSCRIPTION_H