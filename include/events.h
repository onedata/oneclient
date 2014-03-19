/**
 * @file events.h
 * @author Michal Sitko
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_H
#define EVENTS_H

#include <string>
#include <list>
#include <map>
#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include "fuse_messages.pb.h"

#define RULE_MANAGER "rule_manager"
#define CLUSTER_RENGINE "cluster_rengine"

#define EVENT_PRODUCER_CONFIG_REQUEST "event_producer_config_request"
#define EVENT_PRODUCER_CONFIG "eventproducerconfig"
#define EVENT_MESSAGE "eventmessage"

namespace veil {
namespace client {

	class Event{
	public:
		std::map<std::string, boost::any> properties;

		static boost::shared_ptr<Event> createMkdirEvent(std::string userId, std::string fileId);
		static boost::shared_ptr<Event> createWriteEvent(std::string userId, std::string fileId, long long bytes);

		Event();
		Event(const Event & anotherEvent);

		virtual boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> createProtoMessage();

		template<class T>
        T getProperty(std::string fieldName, T defaultValue){
            try{
                if(properties.find(fieldName) == properties.end()){
                    return defaultValue;
                }else{
                    return boost::any_cast<T>(properties[fieldName]);
                }
	        }catch(boost::bad_any_cast){
	        	// TODO: LOG
                return defaultValue;
            }
        }
	};

	class EventProcessor{
	public:
		std::string processEvent(const Event & event){
			return "";
		}
	};

	//rename to EventStream (interface with static method is not something anybody expects)
	class IEventStream{
	public:
		IEventStream();
		IEventStream(boost::shared_ptr<IEventStream> wrappedStream);
    	virtual ~IEventStream();

    	virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event);

    	virtual void setWrappedStream(boost::shared_ptr<IEventStream> wrappedStream);
    	virtual boost::shared_ptr<IEventStream> getWrappedStream();

    protected:
		boost::shared_ptr<IEventStream> m_wrappedStream;

    	virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event) = 0;
	};

	class TrivialEventStream : public IEventStream {
	public:
		TrivialEventStream();
		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);
	};

	class EventFilter : public IEventStream {
	public:
		EventFilter(std::string fieldName, std::string desiredValue);
		EventFilter(boost::shared_ptr<IEventStream> wrappedStream, std::string fieldName, std::string desiredValue);
		static boost::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventFilterConfig & config);

		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

		// for unit test purposes
		std::string getFieldName();
		std::string getDesiredValue();

	private:
		std::string m_fieldName;

		// TODO: type of m_desiredValue hardcoded here and in a few other places so far, it may (but also may not) be a good idea to parametrize this
		std::string m_desiredValue;
	};

	class EventAggregator : public IEventStream {
	public:

		// TODO: make it less visible
		class ActualEventAggregator{
		public:
			ActualEventAggregator();
			virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event, long long threshold,  std::string fieldName);

		private:
			long long m_counter;

			void resetState();
		};

		//TODO: too many constructors
		EventAggregator(long long threshold);
		EventAggregator(std::string fieldName, long long threshold);
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, long long threshold);
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, std::string fieldName, long long threshold);
		static boost::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventAggregatorConfig & config);

		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

		// for unit test purposes
		std::string getFieldName();
		long long getThreshold();

	private:
		std::string m_fieldName;
		long long m_threshold;
		std::map<std::string, ActualEventAggregator> m_substreams;
	};

	class EventStreamCombiner{
	public:
		std::list<boost::shared_ptr<IEventStream> > m_substreams;
		std::list<boost::shared_ptr<Event> > processEvent(boost::shared_ptr<Event> event);
	};

	class IEventStreamFactory{
	public:
    	static boost::shared_ptr<IEventStream> fromConfig(const veil::protocol::fuse_messages::EventStreamConfig & config){
			boost::shared_ptr<IEventStream> res;

			// this piece of code will need to be updated when new EventConfig type is added
			if(config.has_filter_config()){
				::veil::protocol::fuse_messages::EventFilterConfig cfg = config.filter_config();
			 	res = EventFilter::fromConfig(cfg);
			}else if(config.has_aggregator_config()){
				::veil::protocol::fuse_messages::EventAggregatorConfig cfg = config.aggregator_config();
				res = EventAggregator::fromConfig(cfg);
			}

			if(config.has_wrapped_config()){
				boost::shared_ptr<IEventStream> wrapped = IEventStreamFactory::fromConfig(config.wrapped_config());
				res->setWrappedStream(wrapped);
			}

			return res;
		}
	};
} // namespace client
} // namespace veil

 #endif // EVENTS_H