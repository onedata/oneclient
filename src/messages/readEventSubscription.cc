/**
 * @file readEventSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/readEventSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

ReadEventSubscription::ReadEventSubscription(
    const messages::ProtocolServerMessage &serverMessage)
{
    auto &eventSubscriptionMsg = serverMessage.event_subscription();
    auto &readEventSubscriptionMsg =
        eventSubscriptionMsg.read_event_subscription();
    m_id = readEventSubscriptionMsg.id();
    if (readEventSubscriptionMsg.has_counter_threshold())
        m_counterThreshold.reset(readEventSubscriptionMsg.counter_threshold());
    if (readEventSubscriptionMsg.has_time_threshold())
        m_timeThreshold.reset(std::chrono::milliseconds{
            readEventSubscriptionMsg.time_threshold()});
    if (readEventSubscriptionMsg.has_size_threshold())
        m_sizeThreshold.reset(readEventSubscriptionMsg.size_threshold());
}

ReadEventSubscription::ReadEventSubscription(uint64_t i, size_t counterThresh,
    std::chrono::milliseconds timeThresh, size_t sizeThresh)
    : m_id{i}
    , m_counterThreshold{counterThresh}
    , m_timeThreshold{std::move(timeThresh)}
    , m_sizeThreshold{sizeThresh}
{
}

uint64_t ReadEventSubscription::id() const { return m_id; }

const boost::optional<size_t> &ReadEventSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
ReadEventSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

const boost::optional<size_t> &ReadEventSubscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

std::string ReadEventSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEventSubscription', counter threshold: ";
    if (m_counterThreshold)
        stream << m_counterThreshold.get();
    else
        stream << "'undefined'";
    stream << ", size threshold: ";
    if (m_sizeThreshold)
        stream << m_sizeThreshold.get() << " bytes";
    else
        stream << "'undefined'";
    stream << ", time threshold: ";
    if (m_timeThreshold)
        stream << m_timeThreshold.get().count() << " ms";
    else
        stream << "'undefined'";
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
