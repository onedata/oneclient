/**
 * EventStreamCombiner class is an event sink. Input event will be processed by all registered substreams.
 * @file eventStreamCombiner.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_STREAM_COMBINER_H
#define EVENT_STREAM_COMBINER_H

#include "events/event.h"
#include "events/IEventStream.h"

#include <boost/shared_ptr.hpp>
#include <string>
#include <queue>

namespace veil {
namespace client {
namespace events {

/**
 * The EventStreamCombiner class.
 * EventStreamCombiner class is an event sink. Input event will be processed by all registered substreams.
 */
class EventStreamCombiner : public ISchedulable{
public:
    std::list<boost::shared_ptr<Event> > processEvent(boost::shared_ptr<Event> event);		   ///< Process input event. Returns list with output events.
                                                                                               ///< Length of list may be up to number of registered substream. If none of substreams returned non-empty event then empty list is returned.
    virtual bool runTask(TaskID taskId, const std::string &arg0, const std::string &arg1, const std::string &arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
    void addSubstream(boost::shared_ptr<IEventStream> substream);							   ///< Adds substream.
    virtual void pushEventToProcess(boost::shared_ptr<Event> event);						   ///< Pushes event to queue m_eventsToProcess.
    std::queue<boost::shared_ptr<Event> > getEventsToProcess() const;						   ///< TODO: probably should be removed or replaced with getQueueSize

private:
    std::queue<boost::shared_ptr<Event> > m_eventsToProcess;								   ///< Queue of events waiting to be processed.
    std::list<boost::shared_ptr<IEventStream> > m_substreams;								   ///< Registred substreams.
    ReadWriteLock m_eventsToProcessLock;

    boost::shared_ptr<Event> getNextEventToProcess();										   ///< Returns next event to process from queue.
    bool processNextEvent();																   ///< Process next event in queue.
};

} // namespace events
} // namespace client
} // namespace veil

#endif // EVENT_STREAM_COMBINER_H
