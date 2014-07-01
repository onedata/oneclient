/**
 * @file eventStreamCombiner.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventStreamCombiner.h"
#include "events/eventCommunicator.h"
#include "fuse_messages.pb.h"

using namespace veil::client::events;
using namespace std;
using namespace veil::protocol::fuse_messages;

EventStreamCombiner::EventStreamCombiner(std::shared_ptr<veil::client::Context> context)
    : m_context{std::move(context)}
{
}

list<std::shared_ptr<Event> > EventStreamCombiner::processEvent(std::shared_ptr<Event> event)
{
    list<std::shared_ptr<Event> > producedEvents;
    for(list<std::shared_ptr<IEventStream> >::iterator it = m_substreams.begin(); it != m_substreams.end(); it++){
        std::shared_ptr<Event> produced = (*it)->processEvent(event);
        if(produced)
            producedEvents.push_back(produced);
    }
    return producedEvents;
}

bool EventStreamCombiner::runTask(TaskID taskId, const string &arg0, const string &arg1, const string &arg2)
{
    switch(taskId){
    case TASK_PROCESS_EVENT:
        return processNextEvent();

    default:
        return false;
    }
}

bool EventStreamCombiner::processNextEvent()
{
    std::shared_ptr<Event> event = getNextEventToProcess();
    if(event){
        list<std::shared_ptr<Event> > processedEvents = processEvent(event);

        for(list<std::shared_ptr<Event> >::iterator it = processedEvents.begin(); it != processedEvents.end(); ++it){
            std::shared_ptr<EventMessage> eventProtoMessage = (*it)->createProtoMessage();

            EventCommunicator::sendEvent(m_context, eventProtoMessage);
        }
    }

    return true;
}

void EventStreamCombiner::pushEventToProcess(std::shared_ptr<Event> eventToProcess)
{
    AutoLock lock(m_eventsToProcessLock, WRITE_LOCK);
    m_eventsToProcess.push(eventToProcess);
}

std::queue<std::shared_ptr<Event> > EventStreamCombiner::getEventsToProcess() const
{
    return m_eventsToProcess;
}

std::shared_ptr<Event> EventStreamCombiner::getNextEventToProcess()
{
    AutoLock lock(m_eventsToProcessLock, WRITE_LOCK);
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
