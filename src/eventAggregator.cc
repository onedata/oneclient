/**
 * @file eventAggregator.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventAggregator.h"

using namespace veil::client::events;
using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;

EventAggregator::EventAggregator(long long threshold, const string & sumFieldName) :
    IEventStream(), m_fieldName(""), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(const string & fieldName, long long threshold, const string & sumFieldName) :
    IEventStream(), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, long long threshold, const string & sumFieldName) :
    IEventStream(wrappedStream), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(boost::shared_ptr<IEventStream> wrappedStream, const string & fieldName, long long threshold, const string & sumFieldName) :
    IEventStream(wrappedStream), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

boost::shared_ptr<IEventStream> EventAggregator::fromConfig(const EventAggregatorConfig & config)
{
    return boost::shared_ptr<IEventStream> (new EventAggregator(config.field_name(), config.threshold(), config.sum_field_name()));
}

boost::shared_ptr<Event> EventAggregator::actualProcessEvent(boost::shared_ptr<Event> event)
{
    string value;
    if(m_fieldName.empty())
        value = "";
    else{
        value = event->getStringProperty(m_fieldName, "");

        // we simply ignores events without field on which we aggregate
        if(value == "")
            return boost::shared_ptr<Event>();
    }

    if(m_substreams.find(value) == m_substreams.end())
        m_substreams[value] = EventAggregator::ActualEventAggregator();

    return m_substreams[value].processEvent(event, m_threshold, m_fieldName, m_sumFieldName);
}

string EventAggregator::getFieldName()
{
    return m_fieldName;
}

string EventAggregator::getSumFieldName()
{
    return m_sumFieldName;
}

long long EventAggregator::getThreshold()
{
    return m_threshold;
}

EventAggregator::ActualEventAggregator::ActualEventAggregator() :
    m_counter(0)
{}

boost::shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(boost::shared_ptr<Event> event, long long threshold, const string & fieldName, const string & sumFieldName)
{
    AutoLock lock(m_aggregatorStateLock, WRITE_LOCK);
    NumericProperty count = event->getNumericProperty(sumFieldName, 1);
    m_counter += count;

    bool forward = m_counter >= threshold;

    if(forward){
        boost::shared_ptr<Event> newEvent (new Event());
        newEvent->setStringProperty(SUM_FIELD_NAME, sumFieldName);
        newEvent->setNumericProperty(sumFieldName, m_counter);
        if(!fieldName.empty()){
            string value = event->getStringProperty(fieldName, "");
            newEvent->setStringProperty(fieldName, value);
        }
        resetState();
        return newEvent;
    }

    return boost::shared_ptr<Event>();
}

void EventAggregator::ActualEventAggregator::resetState()
{
    m_counter = 0;
}
