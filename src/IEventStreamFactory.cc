/**
 * IEventStreamFactory class is responsible for creating IEventStreams from EventStreamConfigs.
 * @file IEventStreamFactory.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/IEventStreamFactory.h"

#include "events/eventAggregator.h"
#include "events/eventFilter.h"
#include "events/eventTransformer.h"
#include "events/IEventStream.h"
#include "fuse_messages.pb.h"

namespace one
{
namespace client
{
namespace events
{

std::shared_ptr<IEventStream> IEventStreamFactory::fromConfig(const one::clproto::fuse_messages::EventStreamConfig & config)
{
    std::shared_ptr<IEventStream> res;

    // this piece of code will need to be updated when new EventConfig type is added
    if(config.has_filter_config()){
        ::one::clproto::fuse_messages::EventFilterConfig cfg = config.filter_config();
        res = EventFilter::fromConfig(cfg);
    }else if(config.has_aggregator_config()){
        ::one::clproto::fuse_messages::EventAggregatorConfig cfg = config.aggregator_config();
        res = EventAggregator::fromConfig(cfg);
    }else if(config.has_transformer_config()){
        ::one::clproto::fuse_messages::EventTransformerConfig cfg = config.transformer_config();
        res = EventTransformer::fromConfig(cfg);
    }

    if(config.has_wrapped_config()){
        std::shared_ptr<IEventStream> wrapped = IEventStreamFactory::fromConfig(config.wrapped_config());
        res->setWrappedStream(wrapped);
    }

    return res;
}

} // namespace events
} // namespace client
} // namespace one
