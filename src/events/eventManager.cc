/**
* @file eventManager.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events/eventManager.h"

#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> context)
{
    auto communicator = std::make_shared<EventCommunicator>(context);
    m_readEventStream =
        std::make_unique<EventStream<ReadEvent>>(context, communicator);
    m_writeEventStream =
        std::make_unique<EventStream<WriteEvent>>(context, communicator);
}

std::unique_ptr<Event> EventManager::createReadEvent(const std::string &fileId,
                                                     off_t offset,
                                                     size_t size) const
{
    return std::make_unique<ReadEvent>(m_readEventStream, fileId, offset, size);
}

std::unique_ptr<Event> EventManager::createWriteEvent(const std::string &fileId,
                                                      off_t offset, size_t size,
                                                      off_t fileSize) const
{
    return std::make_unique<WriteEvent>(m_writeEventStream, fileId, offset,
                                        size, fileSize);
}

std::unique_ptr<Event>
EventManager::createTruncateEvent(const std::string &fileId,
                                  off_t fileSize) const
{
    return std::make_unique<TruncateEvent>(m_writeEventStream, fileId,
                                           fileSize);
}

} // namespace events
} // namespace client
} // namespace one
