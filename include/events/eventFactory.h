/**
* @file eventFactory.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_FACTORY_H
#define ONECLIENT_EVENTS_EVENT_FACTORY_H

#include <memory>

namespace one {
namespace client {
namespace events {

class ReadEvent;
class WriteEvent;
class ReadEventStream;
class WriteEventStream;
class SubscriptionEvent;
class SubscriptionCancellationEvent;

class EventFactory {
public:
    std::unique_ptr<ReadEvent>
    createReadEvent(const std::string &fileId, off_t offset, size_t size,
                    std::weak_ptr<ReadEventStream> stream) const;

    std::unique_ptr<WriteEvent>
    createWriteEvent(const std::string &fileId, off_t offset, size_t size,
                     off_t fileSize,
                     std::weak_ptr<WriteEventStream> stream) const;

    std::unique_ptr<WriteEvent>
    createTruncateEvent(const std::string &fileId, off_t fileSize,
                        std::weak_ptr<WriteEventStream> stream) const;

    std::unique_ptr<SubscriptionEvent>
    createSubscriptionEvent(unsigned long long id) const;

    std::unique_ptr<SubscriptionCancellationEvent>
    createSubscriptionCancellationEvent(unsigned long long id) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif