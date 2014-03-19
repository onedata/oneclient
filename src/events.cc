/**
 * @file veilfs.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events.h"
#include <iostream>

using namespace veil::client;
using namespace std;
using namespace boost;

shared_ptr<Event> Event::createMkdirEvent(string userId, string fileId)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("mkdir_event");
	event->properties["userId"] = userId;
	event->properties["fileId"] = fileId;
	return event;
}

shared_ptr<Event> Event::createWriteEvent(string userId, string fileId, long long bytes)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("write_event");
	event->properties["userId"] = userId;
	event->properties["fileId"] = fileId;
	event->properties["bytes"] = bytes;
	return event;
}

shared_ptr<veil::protocol::fuse_messages::EventMessage> Event::createProtoMessage(){
	using namespace veil::protocol::fuse_messages;
	shared_ptr<EventMessage> eventMessage (new EventMessage());
	string type = getProperty("type", string(""));
	eventMessage->set_type(type);
	return eventMessage;
}

Event::Event(){}

Event::Event(const Event & anotherEvent){
	properties = anotherEvent.properties;
}

TrivialEventStream::TrivialEventStream()
{
}

shared_ptr<Event> TrivialEventStream::actualProcessEvent(shared_ptr<Event> event)
{
	shared_ptr<Event> newEvent (new Event(*event.get()));
	return newEvent;
}


shared_ptr<Event> IEventStream::processEvent(shared_ptr<Event> event){
	if(!event){
		return shared_ptr<Event>();
	}

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

void IEventStream::setWrappedStream(shared_ptr<IEventStream> wrappedStream)
{
	m_wrappedStream = wrappedStream;
}

shared_ptr<IEventStream> IEventStream::getWrappedStream()
{
	return m_wrappedStream;
}

/*
shared_ptr<IEventStream> IEventStream::fromConfig(const ::veil::protocol::fuse_messages::EventStreamConfig & config){
	shared_ptr<IEventStream> res;

	// this piece of code will need to be updated when new EventConfig type is added
	if(config.has_filter_config()){
		::veil::protocol::fuse_messages::EventFilterConfig cfg = config.filter_config();
	 	res = EventFilter::fromConfig(cfg);
	}else if(config.has_aggregator_config()){
		::veil::protocol::fuse_messages::EventAggregatorConfig cfg = config.aggregator_config();
		res = EventAggregator::fromConfig(cfg);
	}

	if(config.has_wrapped_config()){
		shared_ptr<IEventStream> wrapped = IEventStream::fromConfig(config.wrapped_config());
		res->setWrappedStream(wrapped);
	}

	return res;
}*/

/****** EventFilter ******/

EventFilter::EventFilter(string fieldName, string desiredValue) :
	IEventStream(), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

EventFilter::EventFilter(shared_ptr<IEventStream> wrappedStream, std::string fieldName, std::string desiredValue) :
	IEventStream(wrappedStream), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

shared_ptr<IEventStream> EventFilter::fromConfig(const veil::protocol::fuse_messages::EventFilterConfig & config){
	return shared_ptr<IEventStream> (new EventFilter(config.field_name(), config.desired_value()));
}

shared_ptr<Event> EventFilter::actualProcessEvent(shared_ptr<Event> event)
{
	// defaultValue is generated some way because if we set precomputed value it will not work if desiredValue is the same as precomputed value
	string defaultValue = m_desiredValue + "_";
	string value = event->getProperty(m_fieldName, defaultValue);

	if(value == m_desiredValue){
		shared_ptr<Event> newEvent (new Event(*event.get()));
		return newEvent;
	}else{
		return shared_ptr<Event>();
	}
}

string EventFilter::getFieldName()
{
	return m_fieldName;
}

string EventFilter::getDesiredValue()
{
	return m_desiredValue;
}

/****** EventAggregator ******/

EventAggregator::EventAggregator(long long threshold) :
	IEventStream(), m_fieldName(""), m_threshold(threshold)
{
}

EventAggregator::EventAggregator(std::string fieldName, long long threshold) :
	IEventStream(), m_fieldName(fieldName), m_threshold(threshold)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, long long threshold) :
	IEventStream(wrappedStream), m_threshold(threshold)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, std::string fieldName, long long threshold) :
	IEventStream(wrappedStream), m_fieldName(fieldName), m_threshold(threshold)
{
}

shared_ptr<IEventStream> EventAggregator::fromConfig(const veil::protocol::fuse_messages::EventAggregatorConfig & config){
	return shared_ptr<IEventStream> (new EventAggregator(config.field_name(), config.threshold()));
}

/*EventAggregator::EventAggregator(shared_ptr<IEventStream> wrappedStream, long long threshold) :
	m_wrappedStream(), m_threshold(threshold), m_counter(0)
{
}*/

shared_ptr<Event> EventAggregator::actualProcessEvent(shared_ptr<Event> event)
{
	string value;
	if(m_fieldName.empty())
		value = "";
	else{
		value = event->getProperty(m_fieldName, string(""));

		// we simply ignores events without field on which we aggregate
		if(value == "")
			return shared_ptr<Event>();
	}

	if(m_substreams.find(value) == m_substreams.end())
		m_substreams[value] = EventAggregator::ActualEventAggregator();

	return m_substreams[value].processEvent(event, m_threshold, m_fieldName);
}

string EventAggregator::getFieldName()
{
	return m_fieldName;
}

long long EventAggregator::getThreshold()
{
	return m_threshold;
}

EventAggregator::ActualEventAggregator::ActualEventAggregator() :
	m_counter(0)
{}

shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(shared_ptr<Event> event, long long threshold, string fieldName)
{
	long long count = event->getProperty<long long>("count", 1);
	m_counter += count;

	bool forward = m_counter >= threshold;

	if(forward){
		shared_ptr<Event> newEvent (new Event());
		newEvent->properties["count"] = m_counter;
		if(!fieldName.empty()){
			string value = event->getProperty(fieldName, string());
			newEvent->properties[fieldName] = value;
		}
		resetState();
		return newEvent;
	}

	return shared_ptr<Event>();
}

void EventAggregator::ActualEventAggregator::resetState()
{
	m_counter = 0;
}

list<shared_ptr<Event> > EventStreamCombiner::processEvent(shared_ptr<Event> event)
{
	list<shared_ptr<Event> > producedEvents;
	for(list<shared_ptr<IEventStream> >::iterator it = m_substreams.begin(); it != m_substreams.end(); it++){
		shared_ptr<Event> produced = (*it)->processEvent(event);
		if(produced)
			producedEvents.push_back(produced);
	}
	return producedEvents;
}
