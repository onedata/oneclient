/**
 * @file fileLocationChangedSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_CHANGED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_CHANGED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

#include <chrono>

namespace one {
namespace client {
namespace events {

class FileLocationChanged;

/**
 * @c FileLocationChangedSubscription represents a subscription for a file
 * location change events that occure in the system.
 */
class FileLocationChangedSubscription : public RemoteSubscription {
public:
    /**
     * Constructor.
     * @param fileUuid An UUID of a file for which location change events
     * should be handled.
     * @param handler A callback function that should be executed when file
     * location change events occure.
     */
    FileLocationChangedSubscription(std::string fileUuid,
        std::chrono::milliseconds remoteThreshold,
        EventHandler<FileLocationChanged> handler);

    StreamKey streamKey() const override;

    /**
     * Creates a stream that aggregates event as long as time thresholds is not
     * exceeded.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_fileUuid;
    std::chrono::milliseconds m_remoteThreshold;
    EventHandler<FileLocationChanged> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_LOCATION_CHANGED_SUBSCRIPTION_H
