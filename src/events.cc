/**
 * @file veilfs.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events.h"
#include "veilfs.h"
#include "communication_protocol.pb.h"

using namespace veil::client;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;

EventCommunicator::EventCommunicator(shared_ptr<EventStreamCombiner> eventsStream) : m_eventsStream(eventsStream)
{
	if(!eventsStream){
		m_eventsStream = shared_ptr<EventStreamCombiner>(new EventStreamCombiner());
	}
}

void EventCommunicator::getEventProducerConfig()
{
    using namespace veil::protocol::communication_protocol;
    
    ClusterMsg clm;
    clm.set_protocol_version(PROTOCOL_VERSION);
    clm.set_synch(true);
    clm.set_module_name(RULE_MANAGER);
    clm.set_message_type(ATOM);
    clm.set_answer_type(EVENT_PRODUCER_CONFIG);
    clm.set_message_decoder_name(COMMUNICATION_PROTOCOL);
    clm.set_answer_decoder_name(FUSE_MESSAGES);

    Atom msg;
    msg.set_value(EVENT_PRODUCER_CONFIG_REQUEST);
    clm.set_input(msg.SerializeAsString());

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();

    Answer ans;
    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "sending message failed: " << (connection ? "failed" : "not needed");
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        LOG(INFO) << "event producer config request sent";
    }

    LOG(INFO) << "Answer from event producer config request: " << ans.worker_answer();

    EventProducerConfig config;
    config.ParseFromString(ans.worker_answer());

    for(int i=0; i<config.event_streams_configs_size(); ++i)
    {
        addEventSubstream(config.event_streams_configs(i));
    }
}

void EventCommunicator::sendEvent(shared_ptr<EventMessage> eventMessage)
{
	using namespace veil::protocol::communication_protocol;
    string encodedEventMessage = eventMessage->SerializeAsString();
    
    ClusterMsg clm;
    clm.set_protocol_version(PROTOCOL_VERSION);
    clm.set_synch(false);
    clm.set_module_name(CLUSTER_RENGINE);
    clm.set_message_type(EVENT_MESSAGE);
    clm.set_answer_type(ATOM);
    clm.set_message_decoder_name(FUSE_MESSAGES);
    clm.set_answer_decoder_name(COMMUNICATION_PROTOCOL);

    clm.set_input(encodedEventMessage);

    LOG(INFO) << "Event message created";

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();

    LOG(INFO) << "Connection selected";

    Answer ans;
    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "sending message failed: " << (connection ? "failed" : "not needed");
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        LOG(INFO) << "Event message sent";
    }
}

bool EventCommunicator::handlePushedConfig(const PushMessage & pushMsg)
{
	LOG(INFO) << "Handles pushed EventProducerConfig";
	EventStreamConfig eventStreamConfig;
    if(!eventStreamConfig.ParseFromString(pushMsg.data())){
    	LOG(WARNING) << "Cannot parse pushed message as EventStreamConfig";
    	return false;
    }

    addEventSubstream(eventStreamConfig);
    return true;
}

void EventCommunicator::addEventSubstream(const EventStreamConfig & eventStreamConfig)
{
	shared_ptr<IEventStream> newStream = IEventStreamFactory::fromConfig(eventStreamConfig);
    if(newStream){
        m_eventsStream->addSubstream(newStream);
        LOG(INFO) << "New EventStream added.";
    }
}

void EventCommunicator::processEvent(shared_ptr<Event> event)
{
	if(event){
		m_eventsStream->pushEventToProcess(event);
		VeilFS::getScheduler()->addTask(Job(time(NULL) + 1, m_eventsStream, ISchedulable::TASK_PROCESS_EVENT));
	}
}

shared_ptr<Event> Event::createMkdirEvent(const string & userId, const string & fileId)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("mkdir_event");
	event->properties["userId"] = userId;
	event->properties["fileId"] = fileId;
	return event;
}

shared_ptr<Event> Event::createWriteEvent(const string & userId, const string & fileId, long long bytes)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("write_event");
	event->properties["userId"] = userId;
	event->properties["fileId"] = fileId;
	event->properties["bytes"] = bytes;
	return event;
}

shared_ptr<EventMessage> Event::createProtoMessage()
{
	shared_ptr<EventMessage> eventMessage (new EventMessage());
	string type = getProperty("type", string(""));
	eventMessage->set_type(type);
	return eventMessage;
}

Event::Event(){}

Event::Event(const Event & anotherEvent)
{
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

/****** EventFilter ******/

EventFilter::EventFilter(string fieldName, string desiredValue) :
	IEventStream(), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

EventFilter::EventFilter(shared_ptr<IEventStream> wrappedStream, std::string fieldName, std::string desiredValue) :
	IEventStream(wrappedStream), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

shared_ptr<IEventStream> EventFilter::fromConfig(const EventFilterConfig & config)
{
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

shared_ptr<IEventStream> EventAggregator::fromConfig(const EventAggregatorConfig & config)
{
	return shared_ptr<IEventStream> (new EventAggregator(config.field_name(), config.threshold()));
}

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
	AutoLock lock(m_counterLock, WRITE_LOCK);
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

bool EventStreamCombiner::runTask(TaskID taskId, string arg0, string arg1, string arg2)
{
	switch(taskId){
	case TASK_PROCESS_EVENT:
		return nextEventTask();

	default:
		return false;
	}
}

bool EventStreamCombiner::nextEventTask()
{
	shared_ptr<Event> event = getNextEventToProcess();
	if(event){
		list<boost::shared_ptr<Event> > processedEvents = processEvent(event);

	    for(list<boost::shared_ptr<Event> >::iterator it = processedEvents.begin(); it != processedEvents.end(); ++it){
	        shared_ptr<EventMessage> eventProtoMessage = (*it)->createProtoMessage();

	        EventCommunicator::sendEvent(eventProtoMessage);
	    }
    }

    return true;
}

void EventStreamCombiner::pushEventToProcess(shared_ptr<Event> eventToProcess)
{
	AutoLock lock(m_eventsToProcessLock, WRITE_LOCK);
	m_eventsToProcess.push(eventToProcess);
}

std::queue<boost::shared_ptr<Event> > EventStreamCombiner::getEventsToProcess() const
{
	return m_eventsToProcess;
}

shared_ptr<Event> EventStreamCombiner::getNextEventToProcess()
{
	AutoLock lock(m_eventsToProcessLock, WRITE_LOCK);
	if(m_eventsToProcess.empty()){
		return shared_ptr<Event>();
	}

	shared_ptr<Event> event = m_eventsToProcess.front();
	m_eventsToProcess.pop();
	return event;
}

void EventStreamCombiner::addSubstream(boost::shared_ptr<IEventStream> substream)
{
	m_substreams.push_back(substream);
}
