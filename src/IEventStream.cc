/**
 * @file IEventStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/IEventStream.h"

#include "fuse_messages.pb.h"

using namespace veil::client::events;
using namespace std;
using namespace veil::protocol::fuse_messages;

std::shared_ptr<Event> IEventStream::processEvent(std::shared_ptr<Event> event)
{
    if(m_wrappedStream){
        std::shared_ptr<Event> processedEvent = m_wrappedStream->processEvent(event);
        if(processedEvent)
            return actualProcessEvent(processedEvent);
        else
            return std::shared_ptr<Event>();
    }else{
        return actualProcessEvent(event);
    }
}

IEventStream::IEventStream(std::shared_ptr<IEventStream> wrappedStream) :
    m_wrappedStream(wrappedStream)
{
}

std::shared_ptr<IEventStream> IEventStream::getWrappedStream() const
{
    return m_wrappedStream;
}

void IEventStream::setWrappedStream(std::shared_ptr<IEventStream> wrappedStream)
{
    m_wrappedStream = wrappedStream;
}
