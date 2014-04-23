/**
 * EventAggregator is event stream that divide stream on smaller substreams according to some FieldName value and forward aggregated event when SumField 
 * exceed Threshold.
 * @file eventAggregator.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_AGGREGATOR_H
#define EVENT_AGGREGATOR_H

#include <string>

#include <boost/shared_ptr.hpp>
#include "fuse_messages.pb.h"
#include "fslogicProxy.h"
#include "events/IEventStream.h"

namespace veil {
namespace client {
namespace events {

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

} // namespace events
} // namespace client
} // namespace veil

 #endif // EVENT_AGGREGATOR_H