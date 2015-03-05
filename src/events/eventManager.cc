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

void EventManager::emitReadEvent(const std::string &fileId, off_t offset,
                                 size_t size) const
{
    ReadEvent event{fileId, offset, size};
    m_readEventStream->push(event);
}

void EventManager::emitWriteEvent(const std::string &fileId, off_t offset,
                                  size_t size, off_t fileSize) const
{
    WriteEvent event{fileId, offset, size, fileSize};
    m_writeEventStream->push(event);
}

void EventManager::emitTruncateEvent(const std::string &fileId,
                                     off_t fileSize) const
{
    TruncateEvent event{fileId, fileSize};
    m_writeEventStream->push(event);
}

} // namespace events
} // namespace client
} // namespace one
