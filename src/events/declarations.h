/**
 * @file declarations.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_DECLARATIONS_H
#define ONECLIENT_EVENTS_DECLARATIONS_H

#include "communication/communicator.h"

#include <tbb/concurrent_hash_map.h>

#include <cassert>
#include <memory>
#include <string>
#include <unordered_set>

namespace one {
namespace clproto {
class Event;
class Events;
class Subscription;
class SubscriptionCancellation;
class ClientMessage;
} // namespace clproto
namespace client {
namespace events {

class Aggregator;
class Emitter;
class Event;
class Handler;
class Stream;
class Subscription;
class SubscriptionHandle;

using Router =
    tbb::concurrent_hash_map<std::string, std::unordered_set<std::int64_t>>;
using RouterAcc = typename Router::accessor;
using RouterConstAcc = typename Router::const_accessor;
using SequencerManager = communication::StreamManager;
using SequencerStream = SequencerManager::Stream;

using AggregatorPtr = std::unique_ptr<Aggregator>;
using EmitterPtr = std::unique_ptr<Emitter>;
using EventPtr = std::shared_ptr<Event>;
using ConstEventPtr = std::shared_ptr<const Event>;
using HandlerPtr = std::unique_ptr<Handler>;
using StreamPtr = std::unique_ptr<Stream>;
using ConstSubscriptionPtr = std::shared_ptr<const Subscription>;
using SubscriptionHandlePtr = std::unique_ptr<SubscriptionHandle>;
using SequencerStreamPtr = std::shared_ptr<SequencerStream>;

using EventHandler = std::function<void(std::vector<EventPtr>)>;

using ProtoEvent = clproto::Event;
using ProtoEvents = clproto::Events;
using ProtoSubscription = clproto::Subscription;
using ProtoCancellation = clproto::SubscriptionCancellation;
using ProtoClient = clproto::ClientMessage;

using ProtoEventPtr = std::unique_ptr<ProtoEvent>;
using ProtoSubscriptionPtr = std::unique_ptr<ProtoSubscription>;
using ProtoCancellationPtr = std::unique_ptr<ProtoCancellation>;
using ProtoClientPtr = std::unique_ptr<ProtoClient>;

template <typename T> std::shared_ptr<T> get(EventPtr base)
{
    auto event = std::dynamic_pointer_cast<T>(base);
    assert(event);
    return event;
}

template <typename T> std::shared_ptr<const T> get(ConstEventPtr base)
{
    auto event = std::dynamic_pointer_cast<const T>(base);
    assert(event);
    return event;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_DECLARATIONS_H
