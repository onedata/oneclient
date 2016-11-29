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

    void emit(ConstEventPtr event);

    std::int64_t createStream(ConstSubscriptionPtr subscription);

    bool removeStream(std::int64_t streamId);

    std::int64_t subscribe(
        std::int64_t streamId, ConstSubscriptionPtr subscription);

    void subscribe(const messages::Configuration &configuration);

    bool unsubscribe(std::int64_t subscriptionId);

    void flush(std::int64_t streamId);

private:
    void handle(const ProtoEvents &msg);
    void handle(const ProtoSubscription &msg);
    void handle(const ProtoCancellation &msg);

    std::int64_t nextStreamId();
    std::int64_t nextSubscriptionId();

    std::int64_t subscribe(std::int64_t streamId, std::int64_t subscriptionId,
        ConstSubscriptionPtr subscription);

    std::atomic<std::int64_t> m_nextStreamId{1};
    std::atomic<std::int64_t> m_nextSubscriptionId{1};

    Router m_router;
    Scheduler &m_scheduler;
    SequencerManager m_sequencerManager;
    SequencerStreamPtr m_sequencerStream;

    tbb::concurrent_hash_map<std::int64_t, StreamPtr> m_streams;
    tbb::concurrent_hash_map<std::int64_t, SubscriptionHandlePtr> m_handles;

    using StreamAcc = typename decltype(m_streams)::accessor;
    using StreamConstAcc = typename decltype(m_streams)::const_accessor;
    using HandleAcc = typename decltype(m_handles)::accessor;
    using HandleConstAcc = typename decltype(m_handles)::const_accessor;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MANAGER_H
