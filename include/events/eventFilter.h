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

#include <memory>
#include <string>

namespace veil {
namespace client {
namespace events {

/**
 * The EventFilter class.
 * EventFilter implements IEventStream. EventFilter filters-in events that satisfy some condition, other events are filtered-out.
 * For now condition is simple fieldName == desiredValue, in future it will be extended.
 */
class EventFilter : public IEventStream {
public:
    EventFilter(const std::string & fieldName, const std::string & desiredValue);
    EventFilter(std::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, const std::string & desiredValue);

    static std::shared_ptr<IEventStream> fromConfig(const ::veil::protocol::fuse_messages::EventFilterConfig & config); ///< Constructs EventFilter for protocol buffer message EventFilterConfig
    virtual std::shared_ptr<Event> actualProcessEvent(std::shared_ptr<Event> event); ///<  Implements pure virtual method IEventStream::actualProcessEvent

    // for unit test purposes
    std::string getFieldName();
    std::string getDesiredValue();

private:
    std::string m_fieldName; ///< FieldName for which desired DesiredValue should be satisfied

    // TODO: type of m_desiredValue hardcoded here and in a few other places so far, it may (but also may not) be a good idea to parametrize this
    std::string m_desiredValue; ///< Expected value.
};

} // namespace events
} // namespace client
} // namespace veil

 #endif // EVENT_FILTER_H
