/**
 * @file fileAttrSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H

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
class FileAttrSubscription : public messages::ClientMessage {
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

    /**
     * @return ID of subscription.
     */
    const std::int64_t id() const;

    /**
     * Sets subscription's ID.
     * @param id ID to be set.
     */
    void id(std::int64_t id);

    /**
     * @return UUID of file associated with update events.
     */
    const std::string fileUuid() const;

    /**
     * @return Counter threshold.
     */
    const boost::optional<std::size_t> &counterThreshold() const;

    /**
     * @return Time threshold.
     */
    const boost::optional<std::chrono::milliseconds> &timeThreshold() const;

    /**
     * @return 'true' if none of the thresholds is set, otherwise 'false'.
     */
    bool empty() const;

    virtual std::string toString() const override;

    std::unique_ptr<one::messages::ProtocolClientMessage>
    serialize() const override;

private:
    std::int64_t m_id;
    std::string m_fileUuid;
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_ATTR_SUBSCRIPTION_H
