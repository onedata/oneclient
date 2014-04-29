/**
 * @file eventFilter.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventFilter.h"

using namespace veil::client::events;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;

EventFilter::EventFilter(const string & fieldName, const string & desiredValue) :
    IEventStream(), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

EventFilter::EventFilter(boost::shared_ptr<IEventStream> wrappedStream, const std::string & fieldName, const std::string & desiredValue) :
    IEventStream(wrappedStream), m_fieldName(fieldName), m_desiredValue(desiredValue)
{
}

boost::shared_ptr<IEventStream> EventFilter::fromConfig(const EventFilterConfig & config)
{
    return boost::shared_ptr<IEventStream> (new EventFilter(config.field_name(), config.desired_value()));
}

boost::shared_ptr<Event> EventFilter::actualProcessEvent(boost::shared_ptr<Event> event)
{
    // defaultValue is generated some way because if we set precomputed value it will not work if desiredValue is the same as precomputed value
    string defaultValue = m_desiredValue + "_";
    string value = event->getStringProperty(m_fieldName, defaultValue);

    if(value == m_desiredValue){
        boost::shared_ptr<Event> newEvent (new Event(*event.get()));
        return newEvent;
    }else{
        return boost::shared_ptr<Event>();
    }
}

string EventFilter::getFieldName()
{
    return m_fieldName;
}

string EventFilter::getDesiredValue()
{
    return m_desiredValue;
}
