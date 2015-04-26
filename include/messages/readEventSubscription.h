/**
* @file readEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_READ_EVENT_SUBSCRIPTION_H
#define ONECLIENT_MESSAGES_READ_EVENT_SUBSCRIPTION_H

#include "messages/serverMessage.h"

#include <chrono>
#include <memory>
#include <cstdint>

namespace one {
namespace client {
namespace events {

class ReadEvent;
template <class EventType> class EventStream;

/**
* The ReadEventSubscription class represents read event subscription request
* sent by the server.
*/
class ReadEventSubscription : public one::messages::ServerMessage {
    friend class events::EventStream<events::ReadEvent>;

public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * ReadEventSubscription counterpart.
     */
    ReadEventSubscription(const messages::ProtocolServerMessage &serverMessage);

    /**
     * Constructor.
     * @param id Subscription ID.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of read bytes before emission
     */
    ReadEventSubscription(uint64_t id, size_t counterThreshold,
        std::chrono::milliseconds timeThreshold, size_t sizeThreshold);

private:
    uint64_t m_id;
    size_t m_counterThreshold = SIZE_MAX;
    std::chrono::milliseconds m_timeThreshold =
        std::chrono::milliseconds::max();
    size_t m_sizeThreshold = SIZE_MAX;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_READ_EVENT_SUBSCRIPTION_H
