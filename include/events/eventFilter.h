/**
 * EventFilter is event stream that filter-in events that satisfy some condition, other events are filtered-out.
 * @file eventFilter.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_FILTER_H
#define EVENT_FILTER_H

#include "fuse_messages.pb.h"
#include "fslogicProxy.h"
#include "events/IEventStream.h"

#include <string>
#include <boost/shared_ptr.hpp>

namespace veil {
namespace client {
namespace events {

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

} // namespace events
} // namespace client
} // namespace veil

 #endif // EVENT_FILTER_H