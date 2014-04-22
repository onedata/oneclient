/**
 * @file IEventStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "IEventStream.h"

using namespace veil::client::events;
using namespace std;
using namespace boost;

shared_ptr<Event> IEventStream::processEvent(shared_ptr<Event> event)
{
	if(m_wrappedStream){
		shared_ptr<Event> processedEvent = m_wrappedStream->processEvent(event);
		if(processedEvent)
			return actualProcessEvent(processedEvent);
		else
			return shared_ptr<Event>();
	}else{
		return actualProcessEvent(event);
	}
}

IEventStream::IEventStream() :
	m_wrappedStream(shared_ptr<IEventStream>())
{
}

IEventStream::IEventStream(shared_ptr<IEventStream> wrappedStream) :
	m_wrappedStream(wrappedStream)
{
}

IEventStream::~IEventStream(){

}

shared_ptr<IEventStream> IEventStream::getWrappedStream() const
{
	return m_wrappedStream;
}

void IEventStream::setWrappedStream(shared_ptr<IEventStream> wrappedStream)
{
	m_wrappedStream = wrappedStream;
}