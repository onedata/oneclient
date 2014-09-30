/**
 * EventStreamCombiner class is an event sink. Input event will be processed by all registered substreams.
 * @file eventStreamCombiner.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENT_STREAM_COMBINER_H
#define ONECLIENT_EVENT_STREAM_COMBINER_H


#include "ISchedulable.h"

#include <list>
#include <memory>
#include <mutex>
#include <string>
#include <queue>

namespace one
{
namespace client
{

class Context;

namespace events
{

class IEventStream;
class Event;

/**
 * The EventStreamCombiner class.
 * EventStreamCombiner class is an event sink. Input event will be processed by all registered substreams.
 */
class EventStreamCombiner: public ISchedulable
{
public:
    EventStreamCombiner(std::shared_ptr<Context> context);

    std::list<std::shared_ptr<Event> > processEvent(std::shared_ptr<Event> event);		   ///< Process input event. Returns list with output events.
                                                                                               ///< Length of list may be up to number of registered substream. If none of substreams returned non-empty event then empty list is returned.
    virtual bool runTask(TaskID taskId, const std::string &arg0, const std::string &arg1, const std::string &arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
    void addSubstream(std::shared_ptr<IEventStream> substream);							   ///< Adds substream.
    virtual void pushEventToProcess(std::shared_ptr<Event> event);						   ///< Pushes event to queue m_eventsToProcess.
    std::queue<std::shared_ptr<Event> > getEventsToProcess() const;						   ///< TODO: probably should be removed or replaced with getQueueSize

private:
    const std::shared_ptr<Context> m_context;
    std::queue<std::shared_ptr<Event> > m_eventsToProcess;								   ///< Queue of events waiting to be processed.
    std::list<std::shared_ptr<IEventStream> > m_substreams;								   ///< Registred substreams.
    std::mutex m_eventsToProcessMutex;

    std::shared_ptr<Event> getNextEventToProcess();										   ///< Returns next event to process from queue.
    bool processNextEvent();																   ///< Process next event in queue.
};

} // namespace events
} // namespace client
} // namespace one


#endif // ONECLIENT_EVENT_STREAM_COMBINER_H
