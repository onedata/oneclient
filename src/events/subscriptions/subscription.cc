/**
 * @file subscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscription.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

Subscription::Subscription(boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_,
    boost::optional<std::size_t> sizeThreshold_)
    : m_counterThreshold{std::move(counterThreshold_)}
    , m_timeThreshold{std::move(timeThreshold_)}
    , m_sizeThreshold{sizeThreshold_}
{
}

const std::int64_t Subscription::id() const { return m_id; }

void Subscription::id(std::int64_t id_) { m_id = id_; }

const boost::optional<std::size_t> &Subscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
Subscription::timeThreshold() const
{
    return m_timeThreshold;
}

const boost::optional<std::size_t> &Subscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

bool Subscription::empty() const
{
    return !(m_counterThreshold || m_timeThreshold || m_sizeThreshold);
}

std::string Subscription::toString(std::string type) const
{
    std::stringstream stream;
    stream << "type: '" << type
           << "', counter threshold: " << m_counterThreshold
           << ", time threshold: ";
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
