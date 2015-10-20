/**
 * @file readSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/subscriptions/readSubscription.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

ReadSubscription::ReadSubscription(
    std::int64_t id_, const ProtocolMessage &message)
    : m_id{id_}
{
    if (message.has_counter_threshold())
        m_counterThreshold.reset(message.counter_threshold());
    if (message.has_time_threshold())
        m_timeThreshold.reset(
            std::chrono::milliseconds{message.time_threshold()});
    if (message.has_size_threshold())
        m_sizeThreshold.reset(message.size_threshold());
}

ReadSubscription::ReadSubscription(std::int64_t id_,
    boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_,
    boost::optional<std::size_t> sizeThreshold_)
    : m_id{id_}
    , m_counterThreshold{std::move(counterThreshold_)}
    , m_timeThreshold{std::move(timeThreshold_)}
    , m_sizeThreshold{sizeThreshold_}
{
}

const std::int64_t ReadSubscription::id() const { return m_id; }

const boost::optional<std::size_t> &ReadSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
ReadSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

const boost::optional<std::size_t> &ReadSubscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

bool ReadSubscription::empty() const
{
    return !(m_counterThreshold || m_timeThreshold || m_sizeThreshold);
}

std::string ReadSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadSubscription', counter threshold: "
           << m_counterThreshold << ", time threshold: ";
    if (m_timeThreshold)
        stream << m_timeThreshold.get().count();
    else
        stream << "--";
    stream << ", size threshold: " << m_sizeThreshold;

    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
