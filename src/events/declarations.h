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
#include "streams.h"

#include <tbb/concurrent_hash_map.h>

#include <cassert>
#include <memory>
#include <string>
#include <unordered_set>

namespace one {
namespace messages {
namespace fuse {
class FileAttr;
class FileLocation;
} // namespace fuse
} // namespace messages
class Scheduler;
namespace clproto {
class Event;
class Events;
class Subscription;
class SubscriptionCancellation;
class ClientMessage;
} // namespace clproto
namespace client {
namespace events {

template <class T> class Aggregator;
template <class T> class Emitter;
template <class T> class Handler;
class Event;
class Manager;
class Stream;
class SharedStream;
class Subscription;
class SubscriptionHandle;

using SequencerManager = communication::StreamManager;
using SequencerStream = SequencerManager::Stream;
using FileAttr = messages::fuse::FileAttr;
using FileLocation = messages::fuse::FileLocation;

template <class T> using AggregatorPtr = std::unique_ptr<Aggregator<T>>;
template <class T> using EmitterPtr = std::unique_ptr<Emitter<T>>;
template <class T> using HandlerPtr = std::unique_ptr<Handler<T>>;
template <class T = Event> using EventPtr = std::unique_ptr<T>;
template <class T> using Events = std::vector<EventPtr<T>>;

using StreamPtr = std::unique_ptr<Stream>;
using SharedStreamPtr = std::unique_ptr<SharedStream>;
using SequencerStreamPtr = std::shared_ptr<SequencerStream>;
using SubscriptionHandlePtr = std::unique_ptr<SubscriptionHandle>;

using Streams = tbb::concurrent_hash_map<StreamKey, SharedStreamPtr>;
using StreamAcc = typename Streams::accessor;
using StreamConstAcc = typename Streams::const_accessor;

template <class T> using EventHandler = std::function<void(Events<T>)>;

using ProtoEvent = clproto::Event;
using ProtoEvents = clproto::Events;
using ProtoSubscription = clproto::Subscription;
using ProtoCancellation = clproto::SubscriptionCancellation;
using ProtoClient = clproto::ClientMessage;

using ProtoEventPtr = std::unique_ptr<ProtoEvent>;
using ProtoSubscriptionPtr = std::unique_ptr<ProtoSubscription>;
using ProtoCancellationPtr = std::unique_ptr<ProtoCancellation>;
using ProtoClientPtr = std::unique_ptr<ProtoClient>;

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_DECLARATIONS_H
