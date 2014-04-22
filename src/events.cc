/**
 * @file events.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events.h"
#include "communication_protocol.pb.h"
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>
#include <map>

using namespace veil::client;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

EventCommunicator::EventCommunicator(shared_ptr<EventStreamCombiner> eventsStream) : m_eventsStream(eventsStream)
{
	if(!eventsStream){
		m_eventsStream = shared_ptr<EventStreamCombiner>(new EventStreamCombiner());
	}
	m_messageBuilder.reset(new MessageBuilder());
}

void EventCommunicator::handlePushedConfig(const Answer &msg)
{
	EventStreamConfig eventStreamConfig;
    if(eventStreamConfig.ParseFromString(msg.worker_answer())){
        addEventSubstreamFromConfig(eventStreamConfig);
    }else{
    	LOG(WARNING) << "Cannot parse pushed message as " << eventStreamConfig.GetDescriptor()->name();
    }
}

void EventCommunicator::handlePushedAtom(const Answer &msg)
{
	Atom atom;
    if(atom.ParseFromString(msg.worker_answer())){
		if(atom.value() == "write_enabled"){
	        m_writeEnabled = true;
	        LOG(INFO) << "writeEnabled true";
	    }else if(atom.value() == "write_disabled"){
	        m_writeEnabled = false;
	        LOG(INFO) << "writeEnabled false";
	    }else if(atom.value() == "test_atom2"){
	        // just for test purposes
	        // do nothing
	    }else if(atom.value() == "test_atom2_ack" && msg.has_message_id() && msg.message_id() < -1){
	        // just for test purposes
	        PushListener::sendPushMessageAck("rule_manager", msg.message_id());
	    }
    }else{
    	LOG(WARNING) << "Cannot parse pushed message as " << atom.GetDescriptor()->name();
    }
}

bool EventCommunicator::pushMessagesHandler(const protocol::communication_protocol::Answer &msg)
{
	string messageType = msg.message_type();

	if(boost::iequals(messageType, EventStreamConfig::descriptor()->name())){
		handlePushedConfig(msg);
    }else if(boost::iequals(messageType, Atom::descriptor()->name())){
		handlePushedAtom(msg);
    }

    return true;
}

void EventCommunicator::configureByCluster()
{
    Atom atom;
    atom.set_value(EVENT_PRODUCER_CONFIG_REQUEST);
    
    ClusterMsg clm = m_messageBuilder->createClusterMessage(RULE_MANAGER, ATOM, COMMUNICATION_PROTOCOL, EVENT_PRODUCER_CONFIG, FUSE_MESSAGES, true, atom.SerializeAsString());

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
        addEventSubstreamFromConfig(config.event_streams_configs(i));
    }
}

void EventCommunicator::sendEvent(shared_ptr<EventMessage> eventMessage)
{
    string encodedEventMessage = eventMessage->SerializeAsString();
    
    MessageBuilder messageBuilder;
    ClusterMsg clm = messageBuilder.createClusterMessage(CLUSTER_RENGINE, EVENT_MESSAGE, FUSE_MESSAGES, ATOM, COMMUNICATION_PROTOCOL, false, encodedEventMessage);

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();

    Answer ans;
    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "sending event message failed";
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        DLOG(INFO) << "Event message sent";
    }
}

void EventCommunicator::addEventSubstream(shared_ptr<IEventStream> newStream)
{
	AutoLock lock(m_eventsStreamLock, WRITE_LOCK);
    m_eventsStream->addSubstream(newStream);
    LOG(INFO) << "New EventStream added to EventCommunicator.";
}

void EventCommunicator::addEventSubstreamFromConfig(const EventStreamConfig & eventStreamConfig)
{
	shared_ptr<IEventStream> newStream = IEventStreamFactory::fromConfig(eventStreamConfig);
    if(newStream){
    	addEventSubstream(newStream);
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

	case TASK_IS_WRITE_ENABLED:
        m_writeEnabled = isWriteEnabled();
        return true;

	default:
		return false;
	}
}

void EventCommunicator::addStatAfterWritesRule(int bytes){
    shared_ptr<IEventStream> filter(new EventFilter("type", "write_event"));
    shared_ptr<IEventStream> aggregator(new EventAggregator(filter, "filePath", bytes, "bytes"));
    shared_ptr<IEventStream> customAction(new CustomActionStream(aggregator, boost::bind(&EventCommunicator::statFromWriteEvent, this, _1)));

    addEventSubstream(customAction);
}

void EventCommunicator::setVeilFS(shared_ptr<VeilFS> veilFS){
	m_veilFS = veilFS;
}

bool EventCommunicator::isWriteEnabled()
{
	return m_writeEnabled;
}

shared_ptr<Event> EventCommunicator::statFromWriteEvent(shared_ptr<Event> event){
    string path = event->getStringProperty("filePath", "");
    if(!path.empty() && m_veilFS){
        m_veilFS->statAndUpdatetimes(path);
    }
    return event;
}

shared_ptr<Event> Event::createMkdirEvent(const string & filePath)
{
	shared_ptr<Event> event (new Event());
	event->m_stringProperties["type"] = "mkdir_event";
	event->m_stringProperties["filePath"] = filePath;
	return event;
}

shared_ptr<Event> Event::createWriteEvent(const string & filePath, long long bytes)
{
	shared_ptr<Event> event (new Event());
	event->m_stringProperties["type"] = string("write_event");
	event->m_stringProperties["filePath"] = filePath;
	event->m_numericProperties["bytes"] = bytes;
	return event;
}

shared_ptr<Event> Event::createReadEvent(const string & filePath, long long bytes)
{
	shared_ptr<Event> event (new Event());
	event->m_stringProperties["type"] = string("read_event");
	event->m_stringProperties["filePath"] = filePath;
	event->m_numericProperties["bytes"] = bytes;
	return event;
}

shared_ptr<Event> Event::createRmEvent(const string & filePath)
{
	shared_ptr<Event> event (new Event());
	event->m_stringProperties["type"] = string("rm_event");
	event->m_stringProperties["filePath"] = filePath;
	return event;
}

shared_ptr<Event> Event::createTruncateEvent(const string & filePath, off_t newSize){
	shared_ptr<Event> event (new Event());
	event->m_stringProperties["type"] = "truncate_event";
	event->m_stringProperties["filePath"] = filePath;
	event->m_stringProperties["newSize"] = "newSize";
	return event;
}

shared_ptr<EventMessage> Event::createProtoMessage()
{
	shared_ptr<EventMessage> eventMessage (new EventMessage());
	for(map<string, string>::iterator it = m_stringProperties.begin(); it != m_stringProperties.end(); ++it){
		eventMessage->add_string_properties_keys(it->first);
		eventMessage->add_string_properties_values(it->second);
	}

	for(map<string, NumericProperty>::iterator it = m_numericProperties.begin(); it != m_numericProperties.end(); ++it){
		eventMessage->add_numeric_properties_keys(it->first);
		eventMessage->add_numeric_properties_values(it->second);
	}
	
	return eventMessage;
}

NumericProperty Event::getNumericProperty(const string & key, const NumericProperty defaultValue){
	map<string, NumericProperty>::iterator it = m_numericProperties.find(key);
	if(it == m_numericProperties.end()){
		return defaultValue;
	}else{
		return it->second;
	}
}

string Event::getStringProperty(const string & key, const string & defaultValue){
	map<string, string>::iterator it = m_stringProperties.find(key);
	if(it == m_stringProperties.end()){
		return defaultValue;
	}else{
		return it->second;
	}
}

Event::Event(){}

Event::Event(const Event & anotherEvent)
{
	m_numericProperties = anotherEvent.m_numericProperties;
	m_stringProperties = anotherEvent.m_stringProperties;
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
	string value = event->getStringProperty(m_fieldName, defaultValue);

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

EventAggregator::EventAggregator(shared_ptr<IEventStream> wrappedStream, long long threshold, const string & sumFieldName) :
	IEventStream(wrappedStream), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(shared_ptr<IEventStream> wrappedStream, const string & fieldName, long long threshold, const string & sumFieldName) :
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
		value = event->getStringProperty(m_fieldName, "");

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
	NumericProperty count = event->getNumericProperty(sumFieldName, 1);
	m_counter += count;

	bool forward = m_counter >= threshold;

	if(forward){
		shared_ptr<Event> newEvent (new Event());
		newEvent->m_stringProperties[SUM_FIELD_NAME] = sumFieldName;
		newEvent->m_numericProperties[sumFieldName] = m_counter;
		if(!fieldName.empty()){
			string value = event->getStringProperty(fieldName, "");
			newEvent->m_stringProperties[fieldName] = value;
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
