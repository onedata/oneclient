/**
 * @file writeSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/serverMessage.h"

#include <boost/optional.hpp>

#include <chrono>
#include <cstddef>
#include <sstream>

namespace one {
namespace clproto {
class WriteSubscription;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c WriteSubscription is a server side subscription and represents a request
 * for write operations details.
 */
class WriteSubscription : public Subscription, public messages::ServerMessage {
public:
    using ProtocolMessage = clproto::WriteSubscription;

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param message Protocol Buffers message representing @c WriteSubscription
     * counterpart.
     */
    WriteSubscription(std::int64_t id, const ProtocolMessage &message);

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of written bytes before emission.
     */
    WriteSubscription(std::int64_t id,
        boost::optional<std::size_t> counterThreshold = {},
        boost::optional<std::chrono::milliseconds> timeThreshold = {},
        boost::optional<std::size_t> sizeThreshold = {});

    virtual std::string toString() const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
