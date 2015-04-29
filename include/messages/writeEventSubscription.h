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

#include <boost/optional.hpp>

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

    /**
     * Converts subscription to string format.
     * @return Subscription in string format.
     */
    std::string toString() const;

    /**
     * @return @c Subscription's id.
     */
    uint64_t id() const;

private:
    uint64_t m_id;
    boost::optional<size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    boost::optional<size_t> m_sizeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H
