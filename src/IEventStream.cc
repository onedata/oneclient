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
using namespace boost;
using namespace veil::protocol::fuse_messages;

boost::shared_ptr<Event> IEventStream::processEvent(boost::shared_ptr<Event> event)
{
    if(m_wrappedStream){
        boost::shared_ptr<Event> processedEvent = m_wrappedStream->processEvent(event);
        if(processedEvent)
            return actualProcessEvent(processedEvent);
        else
            return boost::shared_ptr<Event>();
    }else{
        return actualProcessEvent(event);
    }
}

IEventStream::IEventStream() :
    m_wrappedStream(boost::shared_ptr<IEventStream>())
{
}

IEventStream::IEventStream(boost::shared_ptr<IEventStream> wrappedStream) :
    m_wrappedStream(wrappedStream)
{
}

IEventStream::~IEventStream(){

}

boost::shared_ptr<IEventStream> IEventStream::getWrappedStream() const
{
    return m_wrappedStream;
}

void IEventStream::setWrappedStream(boost::shared_ptr<IEventStream> wrappedStream)
{
    m_wrappedStream = wrappedStream;
}
