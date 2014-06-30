/**
 * IEventStreamFactory class is responsible for creating IEventStreams from EventStreamConfigs.
 * @file IEventStreamFactory.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef IEVENT_STREAM_FACTORY_H
#define IEVENT_STREAM_FACTORY_H

#include "events/IEventStream.h"
#include "events/eventFilter.h"
#include "events/eventAggregator.h"
#include "events/eventTransformer.h"

#include <memory>

namespace veil {
namespace client {
namespace events {
    class IEventStream;

    /**
     * The IEventStreamFactory class.
     * EventAggregator is factory for creating IEventStream derived objects from protocol message EventStreamConfig.
     */
    class IEventStreamFactory{
    public:

        /**
         * Creates IEventStream derived object from protocol message EventStreamConfig.
         */
        static std::shared_ptr<IEventStream> fromConfig(const veil::protocol::fuse_messages::EventStreamConfig & config){
            std::shared_ptr<IEventStream> res;

            // this piece of code will need to be updated when new EventConfig type is added
            if(config.has_filter_config()){
                ::veil::protocol::fuse_messages::EventFilterConfig cfg = config.filter_config();
                res = EventFilter::fromConfig(cfg);
            }else if(config.has_aggregator_config()){
                ::veil::protocol::fuse_messages::EventAggregatorConfig cfg = config.aggregator_config();
                res = EventAggregator::fromConfig(cfg);
            }else if(config.has_transformer_config()){
                ::veil::protocol::fuse_messages::EventTransformerConfig cfg = config.transformer_config();
                res = EventTransformer::fromConfig(cfg);
            }

            if(config.has_wrapped_config()){
                std::shared_ptr<IEventStream> wrapped = IEventStreamFactory::fromConfig(config.wrapped_config());
                res->setWrappedStream(wrapped);
            }

            return res;
        }
    };

} // namespace events
} // namespace client
} // namespace veil

#endif // IEVENT_STREAM_FACTORY_H
