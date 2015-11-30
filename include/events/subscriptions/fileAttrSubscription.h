/**
 * @file fileAttrSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/clientMessage.h"

#include <boost/optional.hpp>

#include <chrono>
#include <cstddef>
#include <sstream>

namespace one {
namespace client {
namespace events {

/**
 * @c FileAttrSubscription is a client side subscription and represents a
 * request for file attributes updates.
 */
class FileAttrSubscription : public Subscription,
                             public messages::ClientMessage {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of file for which attributes updates are requested.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     */
    FileAttrSubscription(std::string fileUuid,
        boost::optional<std::size_t> counterThreshold = {},
        boost::optional<std::chrono::milliseconds> timeThreshold = {});

    virtual std::string toString() const override;

    std::unique_ptr<one::messages::ProtocolClientMessage>
    serialize() const override;

private:
    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H
