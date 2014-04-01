/**
 * @file events.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events.h"
#include "veilfs.h"
#include "communication_protocol.pb.h"
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>

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

bool EventCommunicator::pushMessagesHandler(const protocol::communication_protocol::Answer &msg)
{
	string messageType = msg.message_type();

	if(boost::iequals(messageType, EventStreamConfig::descriptor()->name())){
        EventStreamConfig eventStreamConfig;
        if(!eventStreamConfig.ParseFromString(msg.worker_answer())){
            LOG(WARNING) << "Cannot parse pushed message as " << eventStreamConfig.GetDescriptor()->name();
            return false;
        }

        addEventSubstream(eventStreamConfig);
    }

    return true;
}

void EventCommunicator::configureByCluster()
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
        LOG(WARNING) << "sending atom eventproducerconfigrequest failed: " << (connection ? "failed" : "not needed");
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        LOG(INFO) << "atom eventproducerconfigrequest sent";
    }

    LOG(INFO) << "eventproducerconfigrequest answer_status: " << ans.answer_status();

    EventProducerConfig config;
    if(!config.ParseFromString(ans.worker_answer())){
    	LOG(WARNING) << "Cannot parse eventproducerconfigrequest answer as EventProducerConfig";
    	return;
    }

    LOG(INFO) << "Fetched EventProducerConfig contains " << config.event_streams_configs_size() << " stream configurations";

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

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();

    Answer ans;
    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "sending event message failed";
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        DLOG(INFO) << "Event message sent";
    }
}

void EventCommunicator::addEventSubstream(const EventStreamConfig & eventStreamConfig)
{
	shared_ptr<IEventStream> newStream = IEventStreamFactory::fromConfig(eventStreamConfig);
    if(newStream){
        m_eventsStream->addSubstream(newStream);
        LOG(INFO) << "New EventStream added to EventCommunicator.";
    }
}

void EventCommunicator::processEvent(shared_ptr<Event> event)
{
	if(event){
		m_eventsStream->pushEventToProcess(event);
		VeilFS::getScheduler()->addTask(Job(time(NULL) + 1, m_eventsStream, ISchedulable::TASK_PROCESS_EVENT));
	}
}

bool EventCommunicator::runTask(TaskID taskId, string arg0, string arg1, string arg2)
{
	switch(taskId)
	{
	case TASK_GET_EVENT_PRODUCER_CONFIG:
        configureByCluster();
        return true;

	default:
		return false;
	}
}

shared_ptr<Event> Event::createMkdirEvent(const string & filePath)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("mkdir_event");
	event->properties["filePath"] = filePath;
	return event;
}

shared_ptr<Event> Event::createWriteEvent(const string & filePath, long long bytes)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("write_event");
	event->properties["filePath"] = filePath;
	event->properties["bytes"] = bytes;
	return event;
}

shared_ptr<Event> Event::createRmEvent(const string & filePath)
{
	shared_ptr<Event> event (new Event());
	event->properties["type"] = string("rm_event");
	event->properties["filePath"] = filePath;
	return event;
}

shared_ptr<EventMessage> Event::createProtoMessage()
{
	shared_ptr<EventMessage> eventMessage (new EventMessage());
	string type = getProperty("type", string(""));
	eventMessage->set_type(type);
	string sumFieldName = getProperty(SUM_FIELD_NAME, string(""));
	if(!sumFieldName.empty()){
		eventMessage->set_count(getProperty<long long>(SUM_FIELD_NAME, 1L));
	}
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

EventAggregator::EventAggregator(long long threshold, const string & sumFieldName) :
	IEventStream(), m_fieldName(""), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(const string & fieldName, long long threshold, const string & sumFieldName) :
	IEventStream(), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, long long threshold, const string & sumFieldName) :
	IEventStream(wrappedStream), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, const string & fieldName, long long threshold, const string & sumFieldName) :
	IEventStream(wrappedStream), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

shared_ptr<IEventStream> EventAggregator::fromConfig(const EventAggregatorConfig & config)
{
	return shared_ptr<IEventStream> (new EventAggregator(config.field_name(), config.threshold(), config.sum_field_name()));
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

	return m_substreams[value].processEvent(event, m_threshold, m_fieldName, m_sumFieldName);
}

string EventAggregator::getFieldName()
{
	return m_fieldName;
}

string EventAggregator::getSumFieldName()
{
	return m_sumFieldName;
}

long long EventAggregator::getThreshold()
{
	return m_threshold;
}

EventAggregator::ActualEventAggregator::ActualEventAggregator() :
	m_counter(0)
{}

shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(shared_ptr<Event> event, long long threshold, const string & fieldName, const string & sumFieldName)
{
	AutoLock lock(m_aggregatorStateLock, WRITE_LOCK);
	long long count = event->getProperty<long long>(sumFieldName, 1);
	m_counter += count;

	bool forward = m_counter >= threshold;

	if(forward){
		shared_ptr<Event> newEvent (new Event());
		newEvent->properties[SUM_FIELD_NAME] = sumFieldName;
		newEvent->properties[sumFieldName] = m_counter;
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

	for(int i=0; i<m_fieldNamesToReplace.size(); ++i)
	{
		string fieldName = newEvent->getProperty(m_fieldNamesToReplace[i], string());
		if(!fieldName.empty()){
			if(newEvent->getProperty(fieldName, string("")) == m_valuesToReplace[i]){
				newEvent->properties[fieldName] = m_newValues[i];
			}
		}
	}
	return newEvent;
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
