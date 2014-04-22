/**
 * @file events.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "IEventStreamFactory.h"
#include "eventStreamCombiner.h"
#include "eventCommunicator.h"

#include "communication_protocol.pb.h"
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>
#include <map>

using namespace veil::client;
using namespace veil::client::events;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

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
