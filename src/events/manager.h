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

class Manager {
public:
    Manager(std::shared_ptr<Context> context);

    virtual ~Manager() = default;

    void emit(EventPtr<> event);

    template <class T, class... Args> void emit(Args &&... args);

    virtual std::int64_t subscribe(const Subscription &subscription);

    void subscribe(const messages::Configuration &configuration);

    virtual bool unsubscribe(std::int64_t subscriptionId);

    bool existsSubscription(std::int64_t subscriptionId);

    virtual void flush(StreamKey streamKey);

private:
    void handle(const ProtoEvents &msg);
    void handle(const ProtoSubscription &msg);
    void handle(const ProtoCancellation &msg);

    std::int64_t subscribe(
        std::int64_t subscriptionId, const Subscription &subscription);

    std::atomic<std::int64_t> m_nextSubscriptionId{0};

    Streams m_streams;
    Scheduler &m_scheduler;
    SequencerManager m_sequencerManager;
    SequencerStreamPtr m_sequencerStream;
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
