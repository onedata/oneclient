/**
 * @file subscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H

#include <boost/optional.hpp>

#include <chrono>
#include <cstddef>

namespace one {
namespace client {
namespace events {

/**
 * @c Subscription represents subscription for events. It defines minimal
 * requirements that have to be met before events emission.
 */
class Subscription {
public:
    /**
     * Constructor.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of read bytes before emission.
     */
    Subscription(boost::optional<std::size_t> counterThreshold = {},
        boost::optional<std::chrono::milliseconds> timeThreshold = {},
        boost::optional<std::size_t> sizeThreshold = {});

    /**
     * Constructor.
     * @param id ID to be set.
     * @param counterThreshold Maximal number of aggregated events before
     * emission.
     * @param timeThreshold Maximal delay in milliseconds between successive
     * events emissions.
     * @param sizeThreshold Maximal number of read bytes before emission.
     */
    Subscription(std::int64_t id, boost::optional<std::size_t> counterThreshold,
        boost::optional<std::chrono::milliseconds> timeThreshold,
        boost::optional<std::size_t> sizeThreshold);

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

    std::string toString(std::string type) const;

protected:
    std::int64_t m_id;
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    boost::optional<std::size_t> m_sizeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
