/**
 * @file writeSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H

#include "messages/serverMessage.h"

#include <boost/optional.hpp>
#include <boost/optional/optional_io.hpp>

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
class WriteSubscription : public messages::ServerMessage {
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

    /**
     * @return ID of subscription.
     */
    const std::int64_t id() const;

    /**
     * @return Counter threshold.
     */
    const boost::optional<std::size_t> &counterThreshold() const;

    /**
     * @return Time threshold.
     */
    const boost::optional<std::chrono::milliseconds> &timeThreshold() const;

    /**
     * @return Size threshold.
     */
    const boost::optional<std::size_t> &sizeThreshold() const;

    /**
     * @return 'true' if none of the thresholds is set, otherwise 'false'.
     */
    bool empty() const;

    virtual std::string toString() const override;

private:
    std::int64_t m_id;
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    boost::optional<std::size_t> m_sizeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
