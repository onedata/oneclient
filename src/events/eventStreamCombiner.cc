/**
 * @file eventStreamCombiner.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventStreamCombiner.h"

#include "events/event.h"
#include "events/eventCommunicator.h"
#include "events/IEventStream.h"
#include "fuse_messages.pb.h"

using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;

EventStreamCombiner::EventStreamCombiner(std::shared_ptr<one::client::Context> context)
    : m_context{std::move(context)}
{
}

list<std::shared_ptr<Event> > EventStreamCombiner::processEvent(std::shared_ptr<Event> event)
{
    list<std::shared_ptr<Event> > producedEvents;
    for(auto & elem : m_substreams){
        std::shared_ptr<Event> produced = (elem)->processEvent(event);
        if(produced)
            producedEvents.push_back(produced);
    }
    return producedEvents;
}

bool EventStreamCombiner::processNextEvent()
{
    std::shared_ptr<Event> event = getNextEventToProcess();
    if(event){
        list<std::shared_ptr<Event> > processedEvents = processEvent(event);

        for(auto & processedEvent : processedEvents){
            std::shared_ptr<EventMessage> eventProtoMessage = processedEvent->createProtoMessage();

            EventCommunicator::sendEvent(m_context, eventProtoMessage);
        }
    }

    return true;
}

void EventStreamCombiner::sendAllPendingEvents()
{
    for(auto & elem : m_substreams)
    {
        auto producedEvents = elem->getPendingEvents(std::list<std::shared_ptr<Event> >{});

        for(auto & processedEvent : producedEvents)
        {
            std::shared_ptr<EventMessage> eventProtoMessage = processedEvent->createProtoMessage();
            EventCommunicator::sendEvent(m_context, eventProtoMessage);
        }
    }
}


void EventStreamCombiner::pushEventToProcess(std::shared_ptr<Event> eventToProcess)
{
    std::lock_guard<std::mutex> guard{m_eventsToProcessMutex};
    m_eventsToProcess.push(eventToProcess);
}

std::queue<std::shared_ptr<Event> > EventStreamCombiner::getEventsToProcess() const
{
    return m_eventsToProcess;
}

std::shared_ptr<Event> EventStreamCombiner::getNextEventToProcess()
{
    std::lock_guard<std::mutex> guard{m_eventsToProcessMutex};
    if(m_eventsToProcess.empty()){
        return std::shared_ptr<Event>();
    }

    std::shared_ptr<Event> event = m_eventsToProcess.front();
    m_eventsToProcess.pop();
    return event;
}

void EventStreamCombiner::addSubstream(std::shared_ptr<IEventStream> substream)
{
    m_substreams.push_back(substream);
}
