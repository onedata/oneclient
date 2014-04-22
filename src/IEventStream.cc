/**
 * @file IEventStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "IEventStream.h"
#include "fuse_messages.pb.h"

using namespace veil::client::events;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;

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

TrivialEventStream::TrivialEventStream()
{
}

shared_ptr<Event> TrivialEventStream::actualProcessEvent(shared_ptr<Event> event)
{
	shared_ptr<Event> newEvent (new Event(*event.get()));
	return newEvent;
}

EventTransformer::EventTransformer(vector<string> fieldNamesToReplace, vector<string> valuesToReplace, vector<string> newValues) :
	m_fieldNamesToReplace(fieldNamesToReplace), m_valuesToReplace(valuesToReplace), m_newValues(newValues)
{

}

shared_ptr<IEventStream> EventTransformer::fromConfig(const EventTransformerConfig & config)
{
	if(config.field_names_to_replace_size() != config.values_to_replace_size() || config.values_to_replace_size() != config.new_values_size()){
		LOG(WARNING) << "Fields of EventTransformerConfig field_names_to_replace, values_to_replace and new_values are supposed to have the same length";
		return shared_ptr<IEventStream>();
	}
	vector<string> fieldNamesToReplace;
	for(int i=0; i<config.field_names_to_replace_size(); ++i){
		fieldNamesToReplace.push_back(config.field_names_to_replace(i));
	}
	vector<string> valuesToReplace;
	for(int i=0; i<config.values_to_replace_size(); ++i){
		valuesToReplace.push_back(config.values_to_replace(i));
	}
	vector<string> newValues;
	for(int i=0; i<config.new_values_size(); ++i){
		newValues.push_back(config.new_values(i));
	}
	return shared_ptr<IEventStream> (new EventTransformer(fieldNamesToReplace, valuesToReplace, newValues));
}

shared_ptr<Event> EventTransformer::actualProcessEvent(shared_ptr<Event> event)
{
	shared_ptr<Event> newEvent (new Event(*event.get()));

	// TODO: EventTransformer works only for string properties.
	for(int i=0; i<m_fieldNamesToReplace.size(); ++i)
	{
		string fieldName = m_fieldNamesToReplace[i];
		if(newEvent->getStringProperty(fieldName, "") == m_valuesToReplace[i]){
			newEvent->m_stringProperties[fieldName] = m_newValues[i];
		}
	}
	return newEvent;
}

CustomActionStream::CustomActionStream(shared_ptr<IEventStream> wrappedStream, boost::function<shared_ptr<Event>(boost::shared_ptr<Event>)> customActionFun) :
	IEventStream(wrappedStream), m_customActionFun(customActionFun)
{}

shared_ptr<Event> CustomActionStream::actualProcessEvent(shared_ptr<Event> event){
	return m_customActionFun(event);
}