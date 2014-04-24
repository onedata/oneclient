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
using namespace boost;
using namespace veil::protocol::fuse_messages;

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
