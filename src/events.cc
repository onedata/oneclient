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

shared_ptr<Event> TrivialEventStream::processEvent(shared_ptr<Event> event)
{
	if(!event){
		return shared_ptr<Event>();
	}

	shared_ptr<Event> newEvent (new Event(*event.get()));
	return newEvent;
}

/*IEventStream::IEventStream(){}
IEventStream::~IEventStream(){}*/

/****** EventFilter ******/

EventFilter::EventFilter(string fieldName, string desiredValue) :
	m_wrappedStream(), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

EventFilter::EventFilter(shared_ptr<IEventStream> wrappedStream, std::string fieldName, std::string desiredValue) :
	m_wrappedStream(wrappedStream), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

shared_ptr<Event> EventFilter::processEvent(shared_ptr<Event> event)
{

	cout << "BAZINFA!!!!!!!!!!!" << endl;
	if(!event){
		cout << "BAZINFA!!!!!!!!!!! --- !event" << endl;
		return shared_ptr<Event>();
	}

	// defaultValue is generated some way because if we set precomputed value it will not work if desiredValue is the same as precomputed value
	string defaultValue = m_desiredValue + "_";
	string value = event->getProperty(m_fieldName, defaultValue);

	if(value == m_desiredValue){
		shared_ptr<Event> newEvent (new Event(*event.get()));
		return newEvent;
	}else{
		cout << "not equal!!!!!!!!!!!:" << value << ", " << m_desiredValue << endl;
		return shared_ptr<Event>();
	}
}

/****** EventAggregator ******/
/*EventAggregator::EventAggregator(shared_ptr<IEventStream> wrappedStream, long long threshold) :
	m_wrappedStream(), m_threshold(threshold), m_counter(0)
{
}*/

shared_ptr<Event> EventAggregator::processEvent(shared_ptr<Event> event)
{
	if(!event){
		return shared_ptr<Event>();
	}

	//TODO: try catch needed?, handle if does not exists
	long long multiplicity = event->getProperty<long long>("multiplicity", 1);
	m_counter += multiplicity;

	bool forward = m_counter >= m_threshold;

	if(forward){
		shared_ptr<Event> newEvent (new Event());
		newEvent->properties["multiplicity"] = m_counter;
		// TODO:
		//newEvent->properties[fieldName] = 
		resetState();
		return newEvent;
	}else{
		return shared_ptr<Event>();
	}
}

void EventAggregator::resetState(){
	m_counter = 0;
}

/*** EventFloodFilter ***/
EventFloodFilter::EventFloodFilter(shared_ptr<IEventStream> wrappedStream, int minGapInSeconds) :
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
}