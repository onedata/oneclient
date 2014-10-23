/**
 * IEventStreamFactory class is responsible for creating IEventStreams from EventStreamConfigs.
 * @file IEventStreamFactory.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_IEVENT_STREAM_FACTORY_H
#define ONECLIENT_IEVENT_STREAM_FACTORY_H


#include <memory>

namespace one
{

namespace clproto{ namespace fuse_messages{ class EventStreamConfig; }}

namespace client
{
namespace events
{
class IEventStream;

/**
 * The IEventStreamFactory class.
 * EventAggregator is factory for creating IEventStream derived objects from protocol message EventStreamConfig.
 */
class IEventStreamFactory
{
public:

    /**
     * Creates IEventStream derived object from protocol message EventStreamConfig.
     */
    static std::shared_ptr<IEventStream> fromConfig(const one::clproto::fuse_messages::EventStreamConfig & config);
};

} // namespace events
} // namespace client
} // namespace one


#endif // ONECLIENT_IEVENT_STREAM_FACTORY_H
