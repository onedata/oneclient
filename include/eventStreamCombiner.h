/**
 * @file IEventStreamFactory.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_STREAM_COMBINER_H
#define EVENT_STREAM_COMBINER_H

#include "event.h"
#include "IEventStream.h"

#include <boost/shared_ptr.hpp>
#include <string>
#include <queue>

namespace veil {
namespace client {
namespace events {

class EventStreamCombiner : public ISchedulable{
public:
	std::list<boost::shared_ptr<Event> > processEvent(boost::shared_ptr<Event> event);
	virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
	void addSubstream(boost::shared_ptr<IEventStream> substream);
	virtual void pushEventToProcess(boost::shared_ptr<Event> event);
	std::queue<boost::shared_ptr<Event> > getEventsToProcess() const;

private:
	std::queue<boost::shared_ptr<Event> > m_eventsToProcess;
	std::list<boost::shared_ptr<IEventStream> > m_substreams;
	ReadWriteLock m_eventsToProcessLock;

	boost::shared_ptr<Event> getNextEventToProcess();
	bool nextEventTask();
};

} // namespace events
} // namespace client
} // namespace veil

#endif // EVENT_STREAM_COMBINER_H