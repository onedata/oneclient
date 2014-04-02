/**
 * @file events.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_H
#define EVENTS_H

#include <string>
#include <list>
#include <queue>
#include <map>
#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include "glog/logging.h"
#include "fuse_messages.pb.h"
#include "ISchedulable.h"
#include "fslogicProxy.h"

#define RULE_MANAGER "rule_manager"
#define CLUSTER_RENGINE "cluster_rengine"

#define EVENT_PRODUCER_CONFIG_REQUEST "event_producer_config_request"
#define EVENT_PRODUCER_CONFIG "eventproducerconfig"
#define EVENT_MESSAGE "eventmessage"

#define SUM_FIELD_NAME "_sum_field_name"

namespace veil {
namespace client {
	class EventStreamCombiner;

	class Event{
	public:
		std::map<std::string, boost::any> properties;

		static boost::shared_ptr<Event> createMkdirEvent(const std::string & filePath);
		static boost::shared_ptr<Event> createWriteEvent(const std::string & filePath, long long bytes);
		static boost::shared_ptr<Event> createRmEvent(const std::string & filePath);

		Event();
		Event(const Event & anotherEvent);

		virtual boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> createProtoMessage();

		template<class T>
        T getProperty(const std::string & fieldName, const T & defaultValue) {
            try{
                if(properties.find(fieldName) == properties.end()){
                    return defaultValue;
                }else{
                    return boost::any_cast<T>(properties[fieldName]);
                }
	        }catch(boost::bad_any_cast){
	        	LOG(WARNING) << "Bad cast in Event::getProperty for fieldName: " << fieldName;
                return defaultValue;
            }
        }
	};

	class EventCommunicator : public ISchedulable{
	public:
		EventCommunicator(boost::shared_ptr<EventStreamCombiner> eventsStream = boost::shared_ptr<EventStreamCombiner>());

		bool pushMessagesHandler(const protocol::communication_protocol::Answer &msg);
		void addEventSubstream(const ::veil::protocol::fuse_messages::EventStreamConfig & eventStreamConfig);
		void configureByCluster();
		static void sendEvent(boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> eventMessage);
		virtual void processEvent(boost::shared_ptr<Event> event);
		virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

	private:
		boost::shared_ptr<EventStreamCombiner> m_eventsStream;

	};

	class IEventStream {
	public:
		IEventStream();
		IEventStream(boost::shared_ptr<IEventStream> wrappedStream);
    	virtual ~IEventStream();

    	virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event);
    	virtual boost::shared_ptr<IEventStream> getWrappedStream() const;
    	virtual void setWrappedStream(boost::shared_ptr<IEventStream> wrappedStream);

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
			virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event, long long threshold,  const std::string & fieldName, const std::string & sumFieldName);

		private:
			long long m_counter;
			ReadWriteLock m_aggregatorStateLock;

			void resetState();
		};

		//TODO: too many constructors
		EventAggregator(long long threshold, const std::string & sumFieldName = "count");
		EventAggregator(const std::string & fieldName, long long threshold, const std::string & sumFieldName = "count");
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, long long threshold, const std::string & sumFieldName = "count");
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, long long threshold, const std::string & sumFieldName = "count");
		static boost::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventAggregatorConfig & config);

		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

		// for unit test purposes
		std::string getFieldName();
		std::string getSumFieldName();
		long long getThreshold();

	private:
		std::string m_fieldName;
		std::string m_sumFieldName;
		long long m_threshold;
		std::map<std::string, ActualEventAggregator> m_substreams;
	};

	class EventTransformer : public IEventStream {
	public:
		EventTransformer(std::vector<std::string> fieldNamesToReplace, std::vector<std::string> valuesToReplace, std::vector<std::string> newValues);
		static boost::shared_ptr<IEventStream> fromConfig(const :: veil::protocol::fuse_messages::EventTransformerConfig & config);
		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

	private:
		std::vector<std::string> m_fieldNamesToReplace;
		std::vector<std::string> m_valuesToReplace;
		std::vector<std::string> m_newValues;
	};

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
			}else if(config.has_transformer_config()){
				::veil::protocol::fuse_messages::EventTransformerConfig cfg = config.transformer_config();
				res = EventTransformer::fromConfig(cfg);
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