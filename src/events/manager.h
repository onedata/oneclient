/**
 * @file manager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_MANAGER_H
#define ONECLIENT_EVENTS_MANAGER_H

#include "events/declarations.h"
#include "router.h"

#include <atomic>
#include <string>
#include <vector>

namespace one {
class Scheduler;
namespace messages {
class Configuration;
} // namespace messages
namespace client {
class Context;
namespace events {

/**
 * @c Manager is reponsible for creating and managing event streams based on
 * subscriptions and forwarding events to the associated event streams.
 */
class Manager {
    friend class Router;

public:
    /**
     * Constructor.
     * @param context An @c Context instance.
     */
    Manager(std::shared_ptr<Context> context);

    virtual ~Manager() = default;

    /**
     * Forwards event to the associated event stream if present.
     * @param event An event to be forwarded.
     */
    void emit(EventPtr<> event);

    /**
     * A convenience overload function that creates and emits an event.
     * @param args Arguments required to construct an event.
     */
    template <class T, class... Args> void emit(Args &&... args);

    /**
     * Adds subscription and creates associated event stream if not present.
     * @param subscription A subscription to be added.
     * @return A subscription ID, that can be used to cancel subscription.
     */
    virtual std::int64_t subscribe(const Subscription &subscription);

    /**
     * Adds subscriptions based on the remote configuration.
     * @param configuration Remote configuration.
     */
    void subscribe(const messages::Configuration &configuration);

    /**
     * Cancels subscription given by the subscription ID.
     * @param subscriptionId ID of subscription which should be removed.
     * @return True if subscription was found and cancelled, otherwise false.
     */
    virtual bool unsubscribe(std::int64_t subscriptionId);

    /**
     * Checks whether subscription given by the subscription ID exists.
     * @param subscriptionId ID of subscription which existence should be
     * checked.
     * @return True if subscription exists, otherwise false.
     */
    bool existsSubscription(std::int64_t subscriptionId);

    /**
     * Requests handling of events aggregated in all streams.
     */
    void flush();

    /**
     * Requests handling of events aggregated in the stream.
     * @param streamKey A key that identifies a stream that should be flushed.
     */
    virtual void flush(StreamKey streamKey);

private:
    std::int64_t subscribe(
        std::int64_t subscriptionId, const Subscription &subscription);

    std::atomic<std::int64_t> m_nextSubscriptionId{0};

    Streams m_streams;
    Scheduler &m_scheduler;
    SequencerManager m_sequencerManager;
    SequencerStreamPtr m_sequencerStream;
    Router m_router;
    tbb::concurrent_hash_map<std::int64_t, SubscriptionHandlePtr> m_handles;

    using HandleAcc = typename decltype(m_handles)::accessor;
    using HandleConstAcc = typename decltype(m_handles)::const_accessor;
};

template <class T, class... Args> void Manager::emit(Args &&... args)
{
    emit(std::make_unique<T>(std::forward<Args>(args)...));
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MANAGER_H
