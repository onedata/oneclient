/**
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
#include <boost/shared_ptr.hpp>
#include "glog/logging.h"
#include "fuse_messages.pb.h"
#include "ISchedulable.h"
#include "fslogicProxy.h"
#include "event.h"

#define RULE_MANAGER "rule_manager"
#define CLUSTER_RENGINE "cluster_rengine"

#define EVENT_PRODUCER_CONFIG_REQUEST "event_producer_config_request"
#define EVENT_PRODUCER_CONFIG "eventproducerconfig"
#define EVENT_MESSAGE "eventmessage"

#define SUM_FIELD_NAME "_sum_field_name"

namespace veil {
namespace client {
namespace events {

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
		EventFilter(const std::string & fieldName, const std::string & desiredValue);
		EventFilter(boost::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, const std::string & desiredValue);

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
			virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event, NumericProperty threshold,  const std::string & fieldName, const std::string & sumFieldName);

		private:
			NumericProperty m_counter;
			ReadWriteLock m_aggregatorStateLock;

			void resetState();
		};

		//TODO: too many constructors
		EventAggregator(NumericProperty threshold, const std::string & sumFieldName = "count");
		EventAggregator(const std::string & fieldName, NumericProperty threshold, const std::string & sumFieldName = "count");
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, NumericProperty threshold, const std::string & sumFieldName = "count");
		EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, NumericProperty threshold, const std::string & sumFieldName = "count");
		static boost::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventAggregatorConfig & config);

		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

		// for unit test purposes
		std::string getFieldName();
		std::string getSumFieldName();
		NumericProperty getThreshold();

	private:
		std::string m_fieldName;
		std::string m_sumFieldName;
		NumericProperty m_threshold;
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

	class CustomActionStream : public IEventStream {
	public:
		CustomActionStream(boost::shared_ptr<IEventStream> wrappedStream, boost::function<boost::shared_ptr<Event>(boost::shared_ptr<Event>)> customActionFun);
		virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

	private:
		boost::function<boost::shared_ptr<Event>(boost::shared_ptr<Event>)> m_customActionFun;
	};

} // namespace events
} // namespace client
} // namespace veil

 #endif // I_EVENT_STREAM_H