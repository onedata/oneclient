/**
 * @file ioEventSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_IO_EVENT_SUBSCRIPTION_H
#define ONECLIENT_MESSAGES_IO_EVENT_SUBSCRIPTION_H

#include "messages/serverMessage.h"

#include <set>
#include <chrono>
#include <string>
#include <typeinfo>
#include <algorithm>

namespace one {
namespace client {
namespace events {

/**
 * The IOEventSubscription class represents IO event subscription request
 * sent by the server.
 */
template <class EventT>
class IOEventSubscription : public one::messages::ServerMessage {
public:
    IOEventSubscription() = default;

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param subMsg Protocol Buffers message representing @c
     * IOEventSubscription counterpart.
     */
    IOEventSubscription(
        uint64_t id, const typename EventT::Subscription &subMsg);

    /**
     * Constructor.
     * @param id ID of subscription.
     * @param ctrThr Maximal number of aggregated events before emission.
     * @param timeThr Maximal delay in milliseconds between successive events
     * emissions.
     * @param sizeThr Maximal number of read bytes before emission.
     */
    IOEventSubscription(uint64_t id, std::size_t ctrThr,
        std::chrono::milliseconds timeThr, std::size_t sizeThr);

    /**
     * @return ID of subscription.
     */
    const uint64_t id() const;

    /**
     * @return Multiset of aggregated counter thresholds.
     */
    const std::multiset<std::size_t> &counterThresholds() const;

    /**
     * @return Multiset of aggregated time thresholds.
     */
    const std::multiset<std::chrono::milliseconds> &timeThresholds() const;

    /**
     * @return Multiset of aggregated size thresholds.
     */
    const std::multiset<std::size_t> &sizeThresholds() const;

    /**
     * Aggregates this subscription with an other subscription for read events.
     * Aggregation is done by:
     * - union of counter thresholds' multisets
     * - union of time thresholds' multisets
     * - union of size thresholds' multisets
     * @param evtSub IO event subscription to be aggregated.
     * @return @c *this
     */
    IOEventSubscription &operator+=(const IOEventSubscription &evtSub);

    /**
     * Subtracts other subscription from this subscription for read events.
     * Subtraction is done by:
     * - subtraction of counter thresholds' multisets
     * - subtraction of time thresholds' multisets
     * - subtraction of size thresholds' multisets
     * @param evtSub Read event subscription to be subtracted.
     * @return @c *this
     */
    IOEventSubscription &operator-=(const IOEventSubscription &evtSub);

    /**
     * Checks whether all thresholds multisets are empty.
     * @return 'true' if at lease one of counter, time or size thresholds
     * multisets is nonempty, otherwise 'false'.
     */
    bool empty() const;

    /**
     * Checks whether any of provided thresholds is greater than the smallest
     * associated subscription threshold.
     * @param ctrThr Counter threshold to be checked.
     * @param sizeThr Size threshold to be checked.
     * @return 'true' if any of provided thresholds is greater than the smallest
     * associated subscription threshold, otherwise 'false'.
     */
    bool satisfied(std::size_t ctrThr, std::size_t sizeThr) const;

    virtual std::string toString() const override;

private:
    uint64_t m_id;
    std::multiset<std::size_t> m_ctrThrs;
    std::multiset<std::chrono::milliseconds> m_timeThrs;
    std::multiset<std::size_t> m_sizeThrs;
};

template <class EventT>
IOEventSubscription<EventT>::IOEventSubscription(
    uint64_t id_, const typename EventT::Subscription &subMsg)
    : m_id{id_}
{
    if (subMsg.has_counter_threshold())
        m_ctrThrs.emplace(subMsg.counter_threshold());
    if (subMsg.has_time_threshold())
        m_timeThrs.emplace(subMsg.time_threshold());
    if (subMsg.has_size_threshold())
        m_sizeThrs.emplace(subMsg.size_threshold());
}

template <class EventT>
IOEventSubscription<EventT>::IOEventSubscription(uint64_t id_,
    std::size_t ctrThr, std::chrono::milliseconds timeThr, std::size_t sizeThr)
    : m_id{id_}
    , m_ctrThrs{ctrThr}
    , m_timeThrs{std::move(timeThr)}
    , m_sizeThrs{sizeThr}
{
}

template <class EventT> const uint64_t IOEventSubscription<EventT>::id() const
{
    return m_id;
}

template <class EventT>
const std::multiset<std::size_t> &
IOEventSubscription<EventT>::counterThresholds() const
{
    return m_ctrThrs;
}

template <class EventT>
const std::multiset<std::chrono::milliseconds> &
IOEventSubscription<EventT>::timeThresholds() const
{
    return m_timeThrs;
}

template <class EventT>
const std::multiset<std::size_t> &
IOEventSubscription<EventT>::sizeThresholds() const
{
    return m_sizeThrs;
}

template <class EventT>
IOEventSubscription<EventT> &IOEventSubscription<EventT>::operator+=(
    const IOEventSubscription &evtSub)
{
    m_ctrThrs.insert(evtSub.m_ctrThrs.begin(), evtSub.m_ctrThrs.end());
    m_timeThrs.insert(evtSub.m_timeThrs.begin(), evtSub.m_timeThrs.end());
    m_sizeThrs.insert(evtSub.m_sizeThrs.begin(), evtSub.m_sizeThrs.end());
    return *this;
}

template <class EventT>
IOEventSubscription<EventT> &IOEventSubscription<EventT>::operator-=(
    const IOEventSubscription &evtSub)
{
    for (const auto &ctrThr : evtSub.m_ctrThrs)
        m_ctrThrs.erase(m_ctrThrs.find(ctrThr));
    for (const auto &timeThr : evtSub.m_timeThrs)
        m_timeThrs.erase(m_timeThrs.find(timeThr));
    for (const auto &sizeThr : evtSub.m_sizeThrs)
        m_sizeThrs.erase(m_sizeThrs.find(sizeThr));
    return *this;
}

template <class EventT> bool IOEventSubscription<EventT>::empty() const
{
    return m_ctrThrs.empty() && m_timeThrs.empty() && m_sizeThrs.empty();
}

template <class EventT>
bool IOEventSubscription<EventT>::satisfied(
    std::size_t ctrThr, std::size_t sizeThr) const
{
    return (!m_ctrThrs.empty() && *m_ctrThrs.begin() <= ctrThr) ||
        (!m_sizeThrs.empty() && *m_sizeThrs.begin() <= sizeThr);
}

template <class EventT>
std::string IOEventSubscription<EventT>::toString() const
{
    std::stringstream stream;
    stream << "type: '" << EventT::name << "'";
    stream << ", counter thresholds: {";
    for (const auto &ctrThr : m_ctrThrs)
        stream << ctrThr << ", ";
    stream << "}, time thresholds: {";
    for (const auto &timeThr : m_timeThrs)
        stream << timeThr.count() << ", ";
    stream << "}, size thresholds: {";
    for (const auto &sizeThr : m_sizeThrs)
        stream << sizeThr << ", ";
    stream << "}";

    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_IO_EVENT_SUBSCRIPTION_H
