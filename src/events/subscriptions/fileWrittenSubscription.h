/**
 * @file fileWrittenSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_WRITTEN_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_WRITTEN_SUBSCRIPTION_H

#include "subscription.h"

#include <boost/optional.hpp>

#include <chrono>

namespace one {
namespace clproto {
class FileWrittenSubscription;
} // namespace clproto
namespace client {
namespace events {

class FileWrittenSubscription : public Subscription {
    using ProtocolMessage = clproto::FileWrittenSubscription;

public:
    FileWrittenSubscription(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

private:
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_WRITTEN_SUBSCRIPTION_H
