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
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include "fuse_messages.pb.h"

namespace veil {
namespace client {
	/*class Event{
	public:
		time_t timestamp;
	};

	class WriteEvent : public Event{
	public:
		std::string m_userId;
		std::string m_fileId;
		int bytes;
	};

	class MkdirEvent : public Event{
	public:
		MkdirEvent(std::string userId, std::string fileId);
		std::string m_userId;
		std::string m_fileId;
	};*/

	class Event{
	public:
		std::map<std::string, boost::any> properties;
		bool forward;

		/*static boost::shared_ptr<Event> createMkdirEvent(std::string userId, std::string fileId){}
		static boost::shared_ptr<Event> createWriteEvent(std::string userId, std::string fileId, long long bytes);*/

		static boost::shared_ptr<Event> createMkdirEvent(std::string userId, std::string fileId);
		static boost::shared_ptr<Event> createWriteEvent(std::string userId, std::string fileId, long long bytes);

		Event();
		Event(const Event & anotherEvent);

		virtual boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> createProtoMessage();

		template<class T>
        T getProperty(std::string fieldName, T defaultValue){
            try{
                if(properties.find(fieldName) == properties.end()){
                	std::cout << "????? nie znalezino" << std::endl;
                    return defaultValue;
                }else{
                    return boost::any_cast<T>(properties[fieldName]);
                }
	        }catch(boost::bad_any_cast){
	        	// TODO: LOG
                std::cout << "????? zlapano bad_any_cast exception" << std::endl;
                return defaultValue;
            }
        }
	};

	class EventSubscription{
	public:
		std::string m_aggregationFieldName;
		std::string m_function;
		int m_threshold;

		EventSubscription(int threshold);
		EventSubscription(std::string aggregationFieldName, int threshold);
	};

	class EventConfiguration{
	public:
		EventConfiguration(){}
		virtual ~EventConfiguration(){}
		virtual std::list<EventSubscription> getSubscriptions();
		//virtual void addConfigForEvent(std::string event, const EventSubscription & eventSubscription);

	private:
		std::list<EventSubscription> m_eventSubscriptions;
	};

	class EventProcessor{
	public:
		std::string processEvent(const Event & event){
			return "";
		}
	};

	/*class EventAggregationConstraint{
	public:
		EventAggregationConstraint(const std::string & aggregationFieldName = "", const std::string & aggregationValue = "") : 
		m_aggregationFieldName(aggregationFieldName), m_aggregationValue(aggregationValue)
		{
		}

		bool operator<( const EventAggregationConstraint & another ) const {
			if(this->m_aggregationFieldName < another.m_aggregationFieldName)
				return true;

			if(this->m_aggregationValue < another.m_aggregationValue)
				return true;

			return false;
		}

	private: 
		std::string m_eventName;
		std::string m_aggregationFieldName;
		std::string m_aggregationValue;
	};*/

	class IEventStream{
	public:
		IEventStream();
		IEventStream(boost::shared_ptr<IEventStream> wrappedStream);
    	virtual ~IEventStream();

    	virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event);

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

	private:
		std::string m_fieldName;

		// TODO: type of m_desiredValue hardcoded here and in few other places so far, it may (but not necessarily has to) be good idea to parametrize this
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

	private:
		boost::shared_ptr<IEventStream> m_wrappedStream;
		std::string m_fieldName;
		long long m_threshold;
		std::map<std::string, ActualEventAggregator> m_substreams;
	};

	class EventStreamCombiner{
	public:
		//TODO: boost ptr type
		std::list<boost::shared_ptr<IEventStream> > m_substreams;

		std::list<boost::shared_ptr<Event> > processEvent(boost::shared_ptr<Event> event);
	};

	/*class EventFloodFilter : public IEventStream {
	public:
		EventFloodFilter(boost::shared_ptr<IEventStream> wrappedStream, int minGapInSeconds);

		virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event);

	private:
		boost::shared_ptr<IEventStream> m_wrappedStream;
		int m_minGapInSeconds;
	};*/
} // namespace client
} // namespace veil

 #endif // EVENTS_H