/**
 * Class EventCommunicator is facade for event handling module. Contains registered substreams and enables event-related communication with cluster.
 * @file eventCommunicator.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_COMMUNICATOR_H
#define EVENT_COMMUNICATOR_H

#include "veilfs.h"

#include <boost/shared_ptr.hpp>
#include <string>

namespace veil {
namespace client {
class VeilFS;

namespace events {

class EventCommunicator : public ISchedulable{
public:
	EventCommunicator(boost::shared_ptr<EventStreamCombiner> eventsStream = boost::shared_ptr<EventStreamCombiner>());

	void setVeilFS(boost::shared_ptr< ::veil::client::VeilFS> veilFS);
	bool pushMessagesHandler(const protocol::communication_protocol::Answer &msg);
	void addEventSubstream(boost::shared_ptr<IEventStream> eventStreamConfig);
	void addEventSubstreamFromConfig(const ::veil::protocol::fuse_messages::EventStreamConfig & eventStreamConfig);
	void configureByCluster();
	static void sendEvent(boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> eventMessage);
	virtual void processEvent(boost::shared_ptr<Event> event);
	virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
	void addStatAfterWritesRule(int bytes); ///< create and add rule that cause getting attributes and updatetimes after N bytes has been written to single file
	bool isWriteEnabled();

private:
	ReadWriteLock m_eventsStreamLock;
	boost::shared_ptr<EventStreamCombiner> m_eventsStream;
	bool m_writeEnabled;
	boost::shared_ptr<MessageBuilder> m_messageBuilder;
	boost::shared_ptr< ::veil::client::VeilFS> m_veilFS;

	void handlePushedConfig(const veil::protocol::communication_protocol::Answer &msg);
	void handlePushedAtom(const veil::protocol::communication_protocol::Answer &msg);
	boost::shared_ptr<Event> statFromWriteEvent(boost::shared_ptr<Event> event);
};

} // namespace events
} // namespace client
} // namespace veil

#endif // EVENT_COMMUNICATOR_H