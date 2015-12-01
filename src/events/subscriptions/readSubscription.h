/**
 * @file readSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_READ_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_READ_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/serverMessage.h"

#include <boost/optional.hpp>

#include <chrono>
#include <cstddef>
#include <sstream>

namespace one {
namespace clproto {
class ReadSubscription;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c ReadSubscription is a server side subscription and represents a request
 * for read operations details.
 */
class ReadSubscription : public Subscription, public messages::ServerMessage {
public:
    using ProtocolMessage = clproto::ReadSubscription;

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param message Protocol Buffers message representing @c ReadSubscription
     * counterpart.
     */
    ReadSubscription(std::int64_t id, const ProtocolMessage &message);

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of read bytes before emission.
     */
    ReadSubscription(std::int64_t id,
        boost::optional<std::size_t> counterThreshold = {},
        boost::optional<std::chrono::milliseconds> timeThreshold = {},
        boost::optional<std::size_t> sizeThreshold = {});

    virtual std::string toString() const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_READ_SUBSCRIPTION_H
