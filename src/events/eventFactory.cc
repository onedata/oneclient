/**
* @file eventFactory.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events/eventFactory.h"
#include "events/types/event.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/subscriptionEvent.h"
#include "events/types/subscriptionCancellationEvent.h"

namespace one {
namespace client {
namespace events {

std::unique_ptr<ReadEvent>
EventFactory::createReadEvent(const std::string &fileId, off_t offset,
                              size_t size,
                              std::weak_ptr<ReadEventStream> stream) const
{
    return std::make_unique<ReadEvent>(fileId, offset, size, stream);
}

std::unique_ptr<WriteEvent>
EventFactory::createWriteEvent(const std::string &fileId, off_t offset,
                               size_t size, off_t fileSize,
                               std::weak_ptr<WriteEventStream> stream) const
{
    return std::make_unique<WriteEvent>(fileId, offset, size, fileSize, stream);
}

std::unique_ptr<WriteEvent>
EventFactory::createTruncateEvent(const std::string &fileId, off_t fileSize,
                                  std::weak_ptr<WriteEventStream> stream) const
{
    return std::make_unique<WriteEvent>(fileId, 0, 0, fileSize, stream);
}

std::unique_ptr<SubscriptionEvent>
EventFactory::createSubscriptionEvent(unsigned long long id) const
{
    return std::make_unique<SubscriptionEvent>(id);
}

std::unique_ptr<SubscriptionCancellationEvent>
EventFactory::createSubscriptionCancellationEvent(unsigned long long id) const
{
    return std::make_unique<SubscriptionCancellationEvent>(id);
}

} // namespace events
} // namespace client
} // namespace one
