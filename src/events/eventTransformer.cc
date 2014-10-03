/**
 * @file eventTransformer.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventTransformer.h"

#include "events/event.h"
#include "fuse_messages.pb.h"
#include "logging.h"

using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;

EventTransformer::EventTransformer(const vector<string> &fieldNamesToReplace, const vector<string> &valuesToReplace, const vector<string> &newValues) :
    m_fieldNamesToReplace(fieldNamesToReplace), m_valuesToReplace(valuesToReplace), m_newValues(newValues)
{

}

std::shared_ptr<IEventStream> EventTransformer::fromConfig(const EventTransformerConfig & config)
{
    if(config.field_names_to_replace_size() != config.values_to_replace_size() || config.values_to_replace_size() != config.new_values_size()){
        LOG(WARNING) << "Fields of EventTransformerConfig field_names_to_replace, values_to_replace and new_values are supposed to have the same length";
        return std::shared_ptr<IEventStream>();
    }
    vector<string> fieldNamesToReplace;
    for(int i=0; i<config.field_names_to_replace_size(); ++i){
        fieldNamesToReplace.push_back(config.field_names_to_replace(i));
    }
    vector<string> valuesToReplace;
    for(int i=0; i<config.values_to_replace_size(); ++i){
        valuesToReplace.push_back(config.values_to_replace(i));
    }
    vector<string> newValues;
    for(int i=0; i<config.new_values_size(); ++i){
        newValues.push_back(config.new_values(i));
    }
    return std::make_shared<EventTransformer>(fieldNamesToReplace, valuesToReplace, newValues);
}

std::shared_ptr<Event> EventTransformer::actualProcessEvent(std::shared_ptr<Event> event)
{
    auto newEvent = std::make_shared<Event>(*event);

    // TODO: EventTransformer works only for string properties.
    for(size_t i=0; i<m_fieldNamesToReplace.size(); ++i)
    {
        string fieldName = m_fieldNamesToReplace[i];
        if(newEvent->getStringProperty(fieldName, "") == m_valuesToReplace[i]){
            newEvent->setStringProperty(fieldName, m_newValues[i]);
        }
    }
    return newEvent;
}
