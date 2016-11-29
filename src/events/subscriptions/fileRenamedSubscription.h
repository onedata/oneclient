/**
 * @file fileRenamedSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_RENAMED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_RENAMED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

namespace one {
namespace client {
namespace events {

class FileRenamedSubscription : public RemoteSubscription {
public:
    FileRenamedSubscription(std::string fileUuid, EventHandler handler);

    const std::string &routingKey() const override;

    StreamPtr createStream(std::int64_t streamId, Manager &manager,
        SequencerManager &seqManager, Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_fileUuid;
    std::string m_routingKey;
    EventHandler m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_RENAMED_SUBSCRIPTION_H
