/**
 * @file writeSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "writeSubscription.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

WriteSubscription::WriteSubscription(
    std::int64_t id_, const ProtocolMessage &message)
    : Subscription{}
{
    id(id_);
    if (message.has_counter_threshold())
        m_counterThreshold.reset(message.counter_threshold());
    if (message.has_time_threshold())
        m_timeThreshold.reset(
            std::chrono::milliseconds{message.time_threshold()});
    if (message.has_size_threshold())
        m_sizeThreshold.reset(message.size_threshold());
}

WriteSubscription::WriteSubscription(std::int64_t id_,
    boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_,
    boost::optional<std::size_t> sizeThreshold_)
    : Subscription{std::move(counterThreshold_), std::move(timeThreshold_),
          std::move(sizeThreshold_)}
{
    id(id_);
}

std::string WriteSubscription::toString() const
{
    return Subscription::toString("WriteSubscription");
}

} // namespace events
} // namespace client
} // namespace one
