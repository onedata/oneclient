/**
 * @file writeEventSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H
#define ONECLIENT_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H

#include "messages/serverMessage.h"

#include <chrono>
#include <memory>
#include <cstdint>

namespace one {
namespace client {
namespace events {

class WriteEvent;
template <class EventType> class EventStream;

/**
* The WriteEventSubscription class represents write event subscription request
* sent by the server.
*/
class WriteEventSubscription : public one::messages::ServerMessage {
    friend class events::EventStream<events::WriteEvent>;

public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * WriteEventSubscription counterpart.
     */
    WriteEventSubscription(
        const messages::ProtocolServerMessage &serverMessage);

    /**
     * Constructor.
     * @param id Subscription ID.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of read bytes before emission
     */
    WriteEventSubscription(uint64_t id, size_t counterThreshold,
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

#endif // ONECLIENT_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H
