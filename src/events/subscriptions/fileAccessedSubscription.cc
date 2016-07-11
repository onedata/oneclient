/**
 * @file fileAccessedSubscription.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileAccessedSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileAccessedSubscription::FileAccessedSubscription(
    std::int64_t subId, const ProtocolMessage &message)
    : Subscription{subId, {}, {}, {}}
{
    if (message.has_counter_threshold())
        m_counterThreshold.reset(message.counter_threshold());
    if (message.has_time_threshold())
        m_timeThreshold.reset(
            std::chrono::milliseconds{message.time_threshold()});
}

FileAccessedSubscription::FileAccessedSubscription(std::int64_t subId,
    boost::optional<std::size_t> counterThreshold,
    boost::optional<std::chrono::milliseconds> timeThreshold)
    : Subscription{
          subId, std::move(counterThreshold), std::move(timeThreshold), {}}
{
}

std::string FileAccessedSubscription::toString() const
{
    return Subscription::toString("FileAccessedSubscription");
}

} // namespace events
} // namespace client
} // namespace one
