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

EventSubscription::EventSubscription(int threshold) :
	m_aggregationFieldName(""), m_threshold(threshold)
{

}

EventSubscription::EventSubscription(string aggregationFieldName, int threshold) :
	m_aggregationFieldName(aggregationFieldName), m_threshold(threshold)
{

}

list<EventSubscription> EventConfiguration::getSubscriptions(){
	list<EventSubscription> res;
	return res;
}

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

/****** EventFilter ******/

EventFilter::EventFilter(string fieldName, string desiredValue) :
	IEventStream(), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

EventFilter::EventFilter(shared_ptr<IEventStream> wrappedStream, std::string fieldName, std::string desiredValue) :
	IEventStream(wrappedStream), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
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

EventAggregator::ActualEventAggregator::ActualEventAggregator() :
	m_counter(0)
{}

shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(shared_ptr<Event> event, long long threshold, string fieldName){
	long long count = event->getProperty<long long>("count", 1);
	m_counter += count;

	bool forward = m_counter >= threshold;

	if(forward){
		cout << "?????? forwarding: " << m_counter << endl;
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

void EventAggregator::ActualEventAggregator::resetState(){
	m_counter = 0;
}

list<shared_ptr<Event> > EventStreamCombiner::processEvent(shared_ptr<Event> event){
	list<shared_ptr<Event> > producedEvents;
	for(list<shared_ptr<IEventStream> >::iterator it = m_substreams.begin(); it != m_substreams.end(); it++){
		shared_ptr<Event> produced = (*it)->processEvent(event);
		if(produced)
			producedEvents.push_back(produced);
	}
	return producedEvents;
}

/*** EventFloodFilter ***/
/*EventFloodFilter::EventFloodFilter(shared_ptr<IEventStream> wrappedStream, int minGapInSeconds) :
	m_wrappedStream(wrappedStream), m_minGapInSeconds(minGapInSeconds)
{
}

shared_ptr<Event> EventFloodFilter::processEvent(shared_ptr<Event> event)
{
	if(!event){
		return shared_ptr<Event>();
	}

	// TODO: implement
	shared_ptr<Event> newEvent (new Event(*event.get()));
	return newEvent;
}*/