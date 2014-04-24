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

/**
 * The EventAggregator class.
 * EventAggregator is event stream that divide stream on smaller substreams according to some FieldName value and forward aggregated event when SumField 
 * exceeds Threshold.
 */
class EventAggregator : public IEventStream {
public:

	// TODO: make it less visible
	/**
	 * ActualEventAggregator is responsible for holding state of single EventAggregator substream.
	 */
	class ActualEventAggregator{
	public:
		ActualEventAggregator();

		// Process event. Threshold, fieldName and sumFieldName are passed from outside because those parameters are the same for all substreams.
		virtual boost::shared_ptr<Event> processEvent(boost::shared_ptr<Event> event, NumericProperty threshold,  const std::string & fieldName, const std::string & sumFieldName); 

	private:
		NumericProperty m_counter;			 ///< Aggregated value
		ReadWriteLock m_aggregatorStateLock; 

		void resetState();					 ///< Resets state - should be called after every event forward.
	};

	//TODO: too many constructors
	EventAggregator(NumericProperty threshold, const std::string & sumFieldName = "count");
	EventAggregator(const std::string & fieldName, NumericProperty threshold, const std::string & sumFieldName = "count");
	EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, NumericProperty threshold, const std::string & sumFieldName = "count");
	EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, NumericProperty threshold, const std::string & sumFieldName = "count");

	static boost::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventAggregatorConfig & config); ///< Constructs EventFilter for protocol buffer message EventFilterConfig
	virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event); 									  ///<  Implements pure virtual method IEventStream::actualProcessEvent

	// for unit test purposes
	std::string getFieldName();
	std::string getSumFieldName();
	NumericProperty getThreshold();

private:
	std::string m_fieldName; 									///< Name of field by which aggregation is applied, events with same value of this field will be 
																///< placed to the same processing window.
	std::string m_sumFieldName;									///< Name of field for which sum is computed.
	NumericProperty m_threshold;								///< When SumFieldName is greater or equal to threshold aggregated event is emited.
	std::map<std::string, ActualEventAggregator> m_substreams;  ///< Substreams for different value of FieldName (key is FieldName)
};

} // namespace events
} // namespace client
} // namespace veil

 #endif // EVENT_AGGREGATOR_H