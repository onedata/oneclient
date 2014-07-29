/**
 * IEventStream is an abstract class that should inherited by classes that process events.
 * @file IEventStream.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_I_EVENT_STREAM_H
#define VEILCLIENT_I_EVENT_STREAM_H


#include <memory>

namespace veil
{
namespace client
{
namespace events
{

class Event;

static constexpr const char
    *RULE_MANAGER                   = "rule_manager",
    *CLUSTER_RENGINE                = "cluster_rengine",

    *EVENT_PRODUCER_CONFIG_REQUEST  = "event_producer_config_request",
    *EVENT_PRODUCER_CONFIG          = "eventproducerconfig",
    *EVENT_MESSAGE                  = "eventmessage",

    *SUM_FIELD_NAME                 = "_sum_field_name";

/**
 * The IEventStream interface.
 * IEventStream is an abstract class that should inherited by classes that process events.
 * Every IEventStream can have wrappedStream. The most inner object process original event,
 * object that has the most inner object as wrappedStream process event returned by wrappedStream.
 *
 * Classes implementing this interface should implement pure virtual method actualProcessEvent.
 */
class IEventStream
{
public:
    IEventStream() = default;
    IEventStream(std::shared_ptr<IEventStream> wrappedStream);
    virtual ~IEventStream() = default;

    virtual std::shared_ptr<Event> processEvent(std::shared_ptr<Event> event);

    /* Access methods for m_wrappedStream */
    virtual std::shared_ptr<IEventStream> getWrappedStream() const;
    virtual void setWrappedStream(std::shared_ptr<IEventStream> wrappedStream);

protected:
    std::shared_ptr<IEventStream> m_wrappedStream;

    virtual std::shared_ptr<Event> actualProcessEvent(std::shared_ptr<Event> event) = 0; ///< Method to be implemented in derived classes.
                                                                                             ///< Method is called by IEventStream::processEvent only when m_wrappedStream returned non-empty event.
};

} // namespace events
} // namespace client
} // namespace veil


#endif // VEILCLIENT_I_EVENT_STREAM_H
