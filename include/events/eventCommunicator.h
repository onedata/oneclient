/**
 * Class EventCommunicator is facade for event handling module. Contains registered substreams and enables event-related communication with cluster.
 * @file eventCommunicator.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_COMMUNICATOR_H
#define EVENT_COMMUNICATOR_H

#include "fslogicProxy.h"
#include "metaCache.h"
#include "events/event.h"
 #include "events/eventStreamCombiner.h"

#include <boost/shared_ptr.hpp>
#include <string>

namespace veil {
namespace client {
namespace events {

/**
 * The EventCommunicator class.
 * EventCommunicator class is facade for event handling module. Holds registered substreams and enables event-related communication with cluster.
 * Enables registering substreams (addEventSubstream* methods) and handles event-related communication with cluster.
 */
class EventCommunicator : public ISchedulable{
public:
	EventCommunicator(boost::shared_ptr<EventStreamCombiner> eventsStream = boost::shared_ptr<EventStreamCombiner>());

	void addEventSubstream(boost::shared_ptr<IEventStream> eventStreamConfig);  ///< Adds event substream.
	void addEventSubstreamFromConfig(const ::veil::protocol::fuse_messages::EventStreamConfig & eventStreamConfig);
	virtual void processEvent(boost::shared_ptr<Event> event);

	void configureByCluster();				///< Gets streams configuration from cluster, create substreams from fetched configuration and register them.
	bool pushMessagesHandler(const protocol::communication_protocol::Answer &msg); ///< Handles event-related push messages
	virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
	void addStatAfterWritesRule(int bytes); ///< create and add rule that cause getting attributes and updatetimes after N bytes has been written to single file
	bool askClusterIfWriteEnabled(); 		///< Sends to fslogic to get know if writing is enabled. Writing may be disabled if quota is exceeded. 
                                			///< This method is mostly useful on startup, if quota is exeeded during client work cluster will send push message.
	bool isWriteEnabled(); 					///< Getter for m_writeEnabled, does not communicate with cluster.

	/* Access methods */
	void setFslogic(boost::shared_ptr<FslogicProxy> fslogicProxy);
	void setMetaCache(boost::shared_ptr<MetaCache> metaCache);

private:
	ReadWriteLock m_eventsStreamLock;
	boost::shared_ptr<EventStreamCombiner> m_eventsStream;
	bool m_writeEnabled;
	boost::shared_ptr<MessageBuilder> m_messageBuilder;
	boost::shared_ptr<FslogicProxy> m_fslogic;
	boost::shared_ptr<MetaCache> m_metaCache;

	void handlePushedConfig(const veil::protocol::communication_protocol::Answer &msg);
	void handlePushedAtom(const veil::protocol::communication_protocol::Answer &msg);
	boost::shared_ptr<Event> statFromWriteEvent(boost::shared_ptr<Event> event);
};

} // namespace events
} // namespace client
} // namespace veil

#endif // EVENT_COMMUNICATOR_H