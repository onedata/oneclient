/**
 * @file eventAggregator.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventAggregator.h"

#include "events/event.h"
#include "fuse_messages.pb.h"

using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;

EventAggregator::EventAggregator(long long threshold, const string & sumFieldName) :
    IEventStream(), m_fieldName(""), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(const string & fieldName, long long threshold, const string & sumFieldName) :
    IEventStream(), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(std::shared_ptr<IEventStream> wrappedStream, long long threshold, const string & sumFieldName) :
    IEventStream(wrappedStream), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

EventAggregator::EventAggregator(std::shared_ptr<IEventStream> wrappedStream, const string & fieldName, long long threshold, const string & sumFieldName) :
    IEventStream(wrappedStream), m_fieldName(fieldName), m_threshold(threshold), m_sumFieldName(sumFieldName)
{
}

std::shared_ptr<IEventStream> EventAggregator::fromConfig(const EventAggregatorConfig & config)
{
    return std::make_shared<EventAggregator>(config.field_name(), config.threshold(), config.sum_field_name());
}

std::shared_ptr<Event> EventAggregator::actualProcessEvent(std::shared_ptr<Event> event)
{
    string value;
    if(m_fieldName.empty())
        value = "";
    else{
        value = event->getStringProperty(m_fieldName, "");

        // we simply ignores events without field on which we aggregate
        if(value == "")
            return std::shared_ptr<Event>();
    }

    std::lock_guard<std::mutex> guard{m_substreamsMutex};
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

EventAggregator::ActualEventAggregator::ActualEventAggregator(ActualEventAggregator &&other)
    : m_counter{other.m_counter}
{
}

std::shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(std::shared_ptr<Event> event, long long threshold, const string & fieldName, const string & sumFieldName)
{
    std::lock_guard<std::mutex> guard{m_aggregatorStateMutex};
    NumericProperty count = event->getNumericProperty(sumFieldName, 1);
    const auto& blocks = event->getBlocks();
    m_counter += count;
    for(const auto& block : blocks) {
        m_blocks += boost::icl::discrete_interval<off_t>::right_open(block.first, block.first + block.second);
    }

    bool forward = m_counter >= threshold;

    if(forward){
        auto newEvent = std::make_shared<Event>();
        newEvent->setStringProperty(SUM_FIELD_NAME, sumFieldName);
        newEvent->setNumericProperty(sumFieldName, m_counter);
        if(!fieldName.empty()){
            string value = event->getStringProperty(fieldName, "");
            newEvent->setStringProperty(fieldName, value);
        }
        std::list< std::pair<off_t, size_t> > blocks;
        for(const auto& block : m_blocks) {
            blocks.push_back(std::pair<off_t, size_t>(block.lower(), block.upper() - block.lower()));
        }
        newEvent->setBlocks(blocks);
        resetState();
        return newEvent;
    }

    return std::shared_ptr<Event>();
}

void EventAggregator::ActualEventAggregator::resetState()
{
    m_counter = 0;
    m_blocks.clear();
}
