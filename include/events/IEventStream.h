/**
 * IEventStream is an abstract class that should inherited by classes that process events.
 * @file IEventStream.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef I_EVENT_STREAM_H
#define I_EVENT_STREAM_H

#include <string>
#include <list>
#include <queue>
#include <map>
#include <memory>
#include "glog/logging.h"
#include "fuse_messages.pb.h"
#include "ISchedulable.h"
#include "fslogicProxy.h"
#include "events/event.h"

#define RULE_MANAGER "rule_manager"
#define CLUSTER_RENGINE "cluster_rengine"

#define EVENT_PRODUCER_CONFIG_REQUEST "event_producer_config_request"
#define EVENT_PRODUCER_CONFIG "eventproducerconfig"
#define EVENT_MESSAGE "eventmessage"

#define SUM_FIELD_NAME "_sum_field_name"

namespace veil {
namespace client {
namespace events {

    /**
     * The IEventStream interface.
     * IEventStream is an abstract class that should inherited by classes that process events.
     * Every IEventStream can have wrappedStream. The most inner object process original event,
     * object that has the most inner object as wrappedStream process event returned by wrappedStream.
     *
     * Classes implementing this interface should implement pure virtual method actualProcessEvent.
     */
    class IEventStream {
    public:
        IEventStream();
        IEventStream(std::shared_ptr<IEventStream> wrappedStream);
        virtual ~IEventStream();

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

 #endif // I_EVENT_STREAM_H
