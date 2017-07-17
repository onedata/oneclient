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

class FileRenamed;

/**
 * @c FileRenamedSubscription represents a subscription for a rename file
 * operation events that occure in the system.
 */
class FileRenamedSubscription : public RemoteSubscription {
public:
    /**
     * Constructor.
     * @param fileUuid An UUID of a file for which rename operation events
     * should be handled.
     * @param handler A callback function that should be executed when file
     * rename operation events occure.
     */
    FileRenamedSubscription(
        std::string fileUuid, EventHandler<FileRenamed> handler);

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
    EventHandler<FileRenamed> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_RENAMED_SUBSCRIPTION_H
