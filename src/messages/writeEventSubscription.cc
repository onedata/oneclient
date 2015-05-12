/**
 * @file writeEventSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/writeEventSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

WriteEventSubscription::WriteEventSubscription(
    const messages::ProtocolServerMessage &serverMessage)
{
    auto &eventSubscriptionMsg = serverMessage.event_subscription();
    auto &writeEventSubscriptionMsg =
        eventSubscriptionMsg.write_event_subscription();
    m_id = writeEventSubscriptionMsg.id();
    if (writeEventSubscriptionMsg.has_counter_threshold())
        m_counterThreshold.reset(writeEventSubscriptionMsg.counter_threshold());
    if (writeEventSubscriptionMsg.has_time_threshold())
        m_timeThreshold.reset(std::chrono::milliseconds{
            writeEventSubscriptionMsg.time_threshold()});
    if (writeEventSubscriptionMsg.has_size_threshold())
        m_sizeThreshold.reset(writeEventSubscriptionMsg.size_threshold());
}

WriteEventSubscription::WriteEventSubscription(uint64_t id,
    size_t counterThreshold, std::chrono::milliseconds timeThreshold,
    size_t sizeThreshold)
    : m_id{id}
    , m_counterThreshold{counterThreshold}
    , m_timeThreshold{std::move(timeThreshold)}
    , m_sizeThreshold{sizeThreshold}
{
}

uint64_t WriteEventSubscription::id() const { return m_id; }

const boost::optional<size_t> &WriteEventSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
WriteEventSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

const boost::optional<size_t> &WriteEventSubscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

std::string WriteEventSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'WriteEventSubscription', counter threshold: ";
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
