/**
 * @file filePermChangedSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_PERM_CHANGED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_PERM_CHANGED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

namespace one {
namespace client {
namespace events {

class FilePermChanged;

/**
 * @c FilePermChangedSubscription represents a subscription for a file
 * permission change events that occure in the system.
 */
class FilePermChangedSubscription : public RemoteSubscription {
public:
    /**
     * Constructor.
     * @param fileUuid An UUID of a file for which permission change events
     * should be handled.
     * @param handler A callback function that should be executed when file
     * permission change events occure.
     */
    FilePermChangedSubscription(
        std::string fileUuid, EventHandler<FilePermChanged> handler);

    StreamKey streamKey() const override;

    /**
     * Creates a stream that handles each event separately without aggregation.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_fileUuid;
    EventHandler<FilePermChanged> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_PERM_CHANGED_SUBSCRIPTION_H
