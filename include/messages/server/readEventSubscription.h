/**
* @file readEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_SERVER_READ_EVENT_SUBSCRIPTION_H
#define ONECLIENT_MESSAGES_SERVER_READ_EVENT_SUBSCRIPTION_H

#include "serverMessage.h"

#include <chrono>
#include <memory>

namespace one {
namespace client {

namespace events {
class ReadEvent;
template <class EventType, class SubscriptionType> class EventStream;
}

/**
* The ReadEventSubscription class represents read event subscription request
* sent by the server.
*/
class ReadEventSubscription : public ServerMessage {
    friend class events::EventStream<events::ReadEvent, ReadEventSubscription>;

public:
    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * ReadEventSubscription counterpart.
    */
    ReadEventSubscription(std::unique_ptr<ProtocolServerMessage> serverMessage);

private:
    uint64_t m_id;
    size_t m_counterThreshold;
    std::chrono::milliseconds m_timeThreshold;
    size_t m_sizeThreshold;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_SERVER_READ_EVENT_SUBSCRIPTION_H