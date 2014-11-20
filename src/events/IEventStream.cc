/**
 * @file IEventStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/IEventStream.h"

#include "fuse_messages.pb.h"

#include <list>

using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;

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

std::list<std::shared_ptr<Event> > IEventStream::getPendingEvents(std::list<std::shared_ptr<Event> > events)
{
    std::list<std::shared_ptr<Event> > processedEvents;
    for(auto & event : events)
    {
        auto processedEvent = actualProcessEvent(event);
        if(processedEvent)
            processedEvents.push_back(processedEvent);
    }
    return processedEvents;
}


IEventStream::IEventStream(std::shared_ptr<IEventStream> wrappedStream) :
    m_wrappedStream(std::move(wrappedStream))
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
