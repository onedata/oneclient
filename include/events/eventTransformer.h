/**
 * EventTransformer replaces ValuesToReplace of FieldNamesToReplace with NewValues.
 * @file eventTransformer.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_TRANSFORMER_H
#define EVENT_TRANSFORMER_H

#include <string>

#include <boost/shared_ptr.hpp>
#include "fuse_messages.pb.h"
#include "fslogicProxy.h"
#include "events/IEventStream.h"

namespace veil {
namespace client {
namespace events {

/**
 * The EventFilter class.
 * EventFilter implements IEventStream. EventTransformer replaces ValuesToReplace of FieldNamesToReplace with NewValues.
 */
class EventTransformer : public IEventStream {
public:
	EventTransformer(std::vector<std::string> fieldNamesToReplace, std::vector<std::string> valuesToReplace, std::vector<std::string> newValues);

	static boost::shared_ptr<IEventStream> fromConfig(const :: veil::protocol::fuse_messages::EventTransformerConfig & config); ///<  Creates EventTransformer object from protocol buffer message EventTransformerConfig
	virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event); ///<  Implements pure virtual method IEventStream::actualProcessEvent

private:
	std::vector<std::string> m_fieldNamesToReplace;
	std::vector<std::string> m_valuesToReplace;
	std::vector<std::string> m_newValues;
};

} // namespace events
} // namespace client
} // namespace veil

 #endif // EVENT_TRANSFORMER_H