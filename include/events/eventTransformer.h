/**
 * EventTransformer replaces ValuesToReplace of FieldNamesToReplace with NewValues.
 * @file eventTransformer.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_EVENT_TRANSFORMER_H
#define VEILCLIENT_EVENT_TRANSFORMER_H


#include "events/IEventStream.h"

#include <memory>
#include <vector>
#include <string>

namespace veil
{

namespace protocol{ namespace fuse_messages{ class EventTransformerConfig; }}

namespace client
{
namespace events
{

/**
 * The EventFilter class.
 * EventFilter implements IEventStream. EventTransformer replaces ValuesToReplace of FieldNamesToReplace with NewValues.
 */
class EventTransformer: public IEventStream
{
public:
    EventTransformer(const std::vector<std::string> &fieldNamesToReplace, const std::vector<std::string> &valuesToReplace, const std::vector<std::string> &newValues);

    static std::shared_ptr<IEventStream> fromConfig(const :: veil::protocol::fuse_messages::EventTransformerConfig & config); ///<  Creates EventTransformer object from protocol buffer message EventTransformerConfig
    virtual std::shared_ptr<Event> actualProcessEvent(std::shared_ptr<Event> event); ///<  Implements pure virtual method IEventStream::actualProcessEvent

private:
    std::vector<std::string> m_fieldNamesToReplace;
    std::vector<std::string> m_valuesToReplace;
    std::vector<std::string> m_newValues;
};

} // namespace events
} // namespace client
} // namespace veil


#endif // VEILCLIENT_EVENT_TRANSFORMER_H
