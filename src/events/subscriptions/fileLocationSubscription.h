/**
 * @file fileLocationSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

#include <chrono>

namespace one {
namespace client {
namespace events {

class FileLocationSubscription : public RemoteSubscription {
public:
    FileLocationSubscription(std::string fileUuid,
        std::chrono::milliseconds remoteThreshold, EventHandler handler);

    const std::string &routingKey() const override;

    StreamPtr createStream(std::int64_t streamId, Manager &manager,
        SequencerManager &seqManager, Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_fileUuid;
    std::chrono::milliseconds m_remoteThreshold;
    std::string m_routingKey;
    EventHandler m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_SUBSCRIPTION_H
