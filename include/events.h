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
#include "veilfs.h"

#define RULE_MANAGER "rule_manager"
#define CLUSTER_RENGINE "cluster_rengine"

#define EVENT_PRODUCER_CONFIG_REQUEST "event_producer_config_request"
#define EVENT_PRODUCER_CONFIG "eventproducerconfig"
#define EVENT_MESSAGE "eventmessage"

#define SUM_FIELD_NAME "_sum_field_name"

namespace veil {
namespace client {
	typedef long long NumericProperty;

	class IEventStream;
	class EventStreamCombiner;
	class VeilFS;

	class Event{
	public:
		static boost::shared_ptr<Event> createMkdirEvent(const std::string & filePath);
		static boost::shared_ptr<Event> createWriteEvent(const std::string & filePath, NumericProperty bytes);
		static boost::shared_ptr<Event> createReadEvent(const std::string & filePath, NumericProperty bytes);
		static boost::shared_ptr<Event> createRmEvent(const std::string & filePath);
		static boost::shared_ptr<Event> createTruncateEvent(const std::string & filePath, off_t newSize);

		Event();
		Event(const Event & anotherEvent);

		virtual boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> createProtoMessage();

		NumericProperty getNumericProperty(const std::string & key, const NumericProperty defaultValue);
		std::string getStringProperty(const std::string & key, const std::string & defaultValue);

        std::map<std::string, NumericProperty> m_numericProperties;
        std::map<std::string, std::string> m_stringProperties;
	};

	class EventCommunicator : public ISchedulable{
	public:
		EventCommunicator(boost::shared_ptr<EventStreamCombiner> eventsStream = boost::shared_ptr<EventStreamCombiner>());

		void setVeilFS(boost::shared_ptr<VeilFS> veilFS);
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
		boost::shared_ptr<VeilFS> m_veilFS;

		void handlePushedConfig(const veil::protocol::communication_protocol::Answer &msg);
		void handlePushedAtom(const veil::protocol::communication_protocol::Answer &msg);
		boost::shared_ptr<Event> statFromWriteEvent(boost::shared_ptr<Event> event);
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
} // namespace client
} // namespace veil

 #endif // EVENTS_H