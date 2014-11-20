/**
 * Class EventCommunicator is facade for event handling module. Contains registered substreams and enables event-related communication with cluster.
 * @file eventCommunicator.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENT_COMMUNICATOR_H
#define ONECLIENT_EVENT_COMMUNICATOR_H


#include <string>
#include <mutex>
#include <memory>

namespace one
{

namespace clproto
{
namespace fuse_messages
{
class EventStreamConfig;
class EventMessage;
}
namespace communication_protocol
{
class Answer;
}
}

namespace client
{

class Context;
class FslogicProxy;
class MetaCache;

namespace events
{

class IEventStream;
class Event;
class EventStreamCombiner;

/**
 * The EventCommunicator class.
 * EventCommunicator class is facade for event handling module. Holds registered substreams and enables event-related communication with cluster.
 * Enables registering substreams (addEventSubstream* methods) and handles event-related communication with cluster.
 */
class EventCommunicator: public std::enable_shared_from_this<EventCommunicator>
{
public:
    EventCommunicator(std::shared_ptr<Context> context, std::shared_ptr<EventStreamCombiner> eventsStream = std::shared_ptr<EventStreamCombiner>());

    void addEventSubstream(std::shared_ptr<IEventStream> eventStreamConfig);  ///< Adds event substream.
    void addEventSubstreamFromConfig(const ::one::clproto::fuse_messages::EventStreamConfig & eventStreamConfig);
    virtual void processEvent(std::shared_ptr<Event> event);

    void configureByCluster();				///< Gets streams configuration from cluster, create substreams from fetched configuration and register them.
    bool pushMessagesHandler(const clproto::communication_protocol::Answer &msg); ///< Handles event-related push messages
    void addStatAfterWritesRule(int bytes); ///< create and add rule that cause getting attributes and updatetimes after N bytes has been written to single file
    void askIfWriteEnabled();           	///< Sends to fslogic to get know if writing is enabled. Writing may be disabled if quota is exceeded.
                                            ///< This method is mostly useful on startup, if quota is exeeded during client work cluster will send push message.
    bool isWriteEnabled(); 					///< Getter for m_writeEnabled, does not communicate with cluster.
    static void sendEvent(const std::shared_ptr<Context> &context,
                          std::shared_ptr< ::one::clproto::fuse_messages::EventMessage> eventMessage); ///< Sends eventMessage to cluster.
    void scheduleSendingAllPendingEvents();            ///< Schedules sending all pending events.

    /* Access methods */
    void setFslogic(std::shared_ptr<FslogicProxy> fslogicProxy);
    void setMetaCache(std::shared_ptr<MetaCache> metaCache);

private:
    const std::shared_ptr<Context> m_context;

    std::mutex m_eventsStreamMutex;
    std::shared_ptr<EventStreamCombiner> m_eventsStream;
    bool m_writeEnabled;
    std::shared_ptr<FslogicProxy> m_fslogic;
    std::shared_ptr<MetaCache> m_metaCache;

    void handlePushedConfig(const one::clproto::communication_protocol::Answer &msg);
    void handlePushedAtom(const one::clproto::communication_protocol::Answer &msg);
    std::shared_ptr<Event> statFromWriteEvent(std::shared_ptr<Event> event);
};

} // namespace events
} // namespace client
} // namespace one


#endif // ONECLIENT_EVENT_COMMUNICATOR_H
