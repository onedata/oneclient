/**
 * @file fileAccessedSubscription.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ACCESSED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ACCESSED_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/serverMessage.h"

#include <boost/optional.hpp>

#include <chrono>
#include <cstddef>

namespace one {

namespace clproto {
class FileAccessedSubscription;
}

namespace client {
namespace events {

/**
 * @c FileAccessedSubscription is a client side subscription and represents
 * a request for @c FileAccessedEvent.
 */
class FileAccessedSubscription : public Subscription,
                                 public messages::ServerMessage {
public:
    using ProtocolMessage = clproto::FileAccessedSubscription;

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param message Protocol Buffers message representing @c
     * FileAccessedSubscription counterpart.
     */
    FileAccessedSubscription(std::int64_t id, const ProtocolMessage &message);

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     */
    FileAccessedSubscription(std::int64_t id,
        boost::optional<std::size_t> counterThreshold = {},
        boost::optional<std::chrono::milliseconds> timeThreshold = {});

    virtual std::string toString() const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ACCESSED_SUBSCRIPTION_H
