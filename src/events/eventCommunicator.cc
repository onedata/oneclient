/**
 * @file eventCommunicator.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventCommunicator.h"

#include "communication_protocol.pb.h"
#include "communication/communicator.h"
#include "communication/exception.h"
#include "context.h"
#include "events/customActionStream.h"
#include "events/event.h"
#include "events/eventAggregator.h"
#include "events/eventFilter.h"
#include "events/eventStreamCombiner.h"
#include "events/IEventStream.h"
#include "events/IEventStreamFactory.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "logging.h"
#include "metaCache.h"
#include "options.h"
#include "pushListener.h"
#include "scheduler.h"
#include "fsImpl.h"

#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>

#include <functional>
#include <iostream>
#include <memory>

using namespace one::client;
using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;
using namespace one::clproto::communication_protocol;
using namespace std::literals::chrono_literals;

EventCommunicator::EventCommunicator(std::shared_ptr<Context> context, std::shared_ptr<EventStreamCombiner> eventsStream)
    : m_context{std::move(context)}
    , m_eventsStream(eventsStream)
    , m_writeEnabled(true)
{
    shared_from_this();
    if(!eventsStream){
        m_eventsStream = std::make_shared<EventStreamCombiner>(m_context);
    }
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
            m_context->getPushListener()->sendPushMessageAck(msg, communication::ServerModule::RULE_MANAGER);
        }
    }else{
        LOG(WARNING) << "Cannot parse pushed message as " << atom.GetDescriptor()->name();
    }
}

bool EventCommunicator::pushMessagesHandler(const clproto::communication_protocol::Answer &msg)
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

    auto communicator = m_context->getCommunicator();
    try
    {
        auto ans = communicator->communicate<EventProducerConfig>(communication::ServerModule::RULE_MANAGER, atom);
        if(ans->answer_status() == VEIO)
            LOG(WARNING) << "sending atom eventproducerconfigrequest failed: not needed";

        LOG(INFO) << "eventproducerconfigrequest answer_status: " << ans->answer_status();

        EventProducerConfig config;
        if(!config.ParseFromString(ans->worker_answer())){
            LOG(WARNING) << "Cannot parse eventproducerconfigrequest answer as EventProducerConfig";
            return;
        }

        LOG(INFO) << "Fetched EventProducerConfig contains " << config.event_streams_configs_size() << " stream configurations";

        for(int i=0; i<config.event_streams_configs_size(); ++i)
        {
            addEventSubstreamFromConfig(config.event_streams_configs(i));
        }
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "sending atom eventproducerconfigrequest failed: " << e.what();
    }
}

void EventCommunicator::sendEvent(const std::shared_ptr<Context> &context,
                                  std::shared_ptr<EventMessage> eventMessage)
{
    auto communicator = context->getCommunicator();
    try
    {
        auto ans = communicator->communicate<>(communication::ServerModule::CLUSTER_RENGINE, *eventMessage);
        if(ans->answer_status() == VEIO)
            LOG(WARNING) << "sending event message failed";
        else
            DLOG(INFO) << "Event message sent";
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "sending event message failed: " << e.what();
    }
}

void EventCommunicator::askIfWriteEnabled()
{
    Atom atom;
    atom.set_value("is_write_enabled");

    auto communicator = m_context->getCommunicator();
    try
    {
        auto ans = communicator->communicate<>(communication::ServerModule::FSLOGIC, atom);
        if(ans->answer_status() == VEIO)
            LOG(WARNING) << "sending atom is_write_enabled failed";
        else
            LOG(INFO) << "atom is_write_enabled sent";

        Atom response;
        if(!response.ParseFromString(ans->worker_answer()))
        {
            LOG(WARNING) << " cannot parse is_write_enabled response as atom. Using WriteEnabled = true mode.";
            m_writeEnabled = true;
        }
        else
            m_writeEnabled = response.value() != "false";
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "sending atom is_write_enabled failed: " << e.what();
        m_writeEnabled = true;
    }
}

void EventCommunicator::addEventSubstream(std::shared_ptr<IEventStream> newStream)
{
    std::lock_guard<std::mutex> guard{m_eventsStreamMutex};
    m_eventsStream->addSubstream(newStream);
    LOG(INFO) << "New EventStream added to EventCommunicator.";
}

void EventCommunicator::addEventSubstreamFromConfig(const EventStreamConfig & eventStreamConfig)
{
    std::shared_ptr<IEventStream> newStream = IEventStreamFactory::fromConfig(eventStreamConfig);
    if(newStream){
        addEventSubstream(newStream);
    }
}

void EventCommunicator::processEvent(std::shared_ptr<Event> event)
{
    if(event){
        m_eventsStream->pushEventToProcess(event);
        m_context->scheduler()->schedule(
                    1s,
                    &EventStreamCombiner::processNextEvent,
                    m_eventsStream);
    }
}

void EventCommunicator::scheduleSendingAllPendingEvents()
{
    m_eventsStream->sendAllPendingEvents();
    m_context->scheduler()->schedule(
                15s,
                &EventCommunicator::scheduleSendingAllPendingEvents,
                shared_from_this());
    std::cout<<"Send all\n";
}

void EventCommunicator::addStatAfterWritesRule(int bytes){
    auto filter = std::make_shared<EventFilter>("type", "write_event");
    auto aggregator = std::make_shared<EventAggregator>(filter, "filePath", bytes, "bytes");
    auto customAction = std::make_shared<CustomActionStream>(aggregator,
        std::bind(&EventCommunicator::statFromWriteEvent, this, std::placeholders::_1));

    addEventSubstream(customAction);
}

void EventCommunicator::setFslogic(std::shared_ptr<FslogicProxy> fslogicProxy)
{
    m_fslogic = fslogicProxy;
}

void EventCommunicator::setMetaCache(std::shared_ptr<MetaCache> metaCache)
{
    m_metaCache = metaCache;
}

bool EventCommunicator::isWriteEnabled()
{
    return m_writeEnabled;
}

std::shared_ptr<Event> EventCommunicator::statFromWriteEvent(std::shared_ptr<Event> event){
    string path = event->getStringProperty("filePath", "");
    if(!path.empty() && m_metaCache && m_fslogic){
        time_t currentTime = time(nullptr);
        m_fslogic->updateTimes(path, 0, currentTime, currentTime);

        FileAttr attr;
        m_metaCache->clearAttr(path);
        if(m_context->getOptions()->get_enable_attr_cache()){
            // TODO: The whole mechanism we force attributes to be reloaded is inefficient - we just want to cause attributes to be changed on cluster but
            // we also fetch attributes
            m_fslogic->getFileAttr(string(path), attr);
        }
    }

    // we don't want to forward this event - it has already been handled by this function
    return std::shared_ptr<Event> ();
}
