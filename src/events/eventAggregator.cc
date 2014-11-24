/**
 * @file eventAggregator.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/eventAggregator.h"

#include "events/event.h"
#include "fuse_messages.pb.h"

#include <list>
#include <map>

using namespace one::client::events;
using namespace std;
using namespace one::clproto::fuse_messages;



constexpr const char *FILE_PATH = "filePath";

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


std::shared_ptr<Event> EventAggregator::ActualEventAggregator::processEvent(std::shared_ptr<Event> event, long long threshold, const string & fieldName, const string & sumFieldName)
{
    std::lock_guard<std::mutex> guard{m_aggregatorStateMutex};
    NumericProperty count = event->getNumericProperty(sumFieldName, 1);
    const auto& blocks = event->getBlocks();

    std::string path = event->getStringProperty(FILE_PATH, "");
    auto resultCounter = m_counters.emplace(path, 0);
    resultCounter.first->second += count;

    auto resultBlocks = m_blocks.emplace(path, boost::icl::interval_set<off_t>());

    for(const auto& block : blocks) {
        resultBlocks.first->second += boost::icl::discrete_interval<off_t>::right_open(block.first, block.first + block.second);
    }

    bool forward = resultCounter.first->second >= threshold;

    if(forward){
        auto newEvent = std::make_shared<Event>();
        newEvent->setStringProperty(SUM_FIELD_NAME, sumFieldName);
        newEvent->setNumericProperty(sumFieldName, resultCounter.first->second);
        if(!fieldName.empty()){
            string value = event->getStringProperty(fieldName, "");
            newEvent->setStringProperty(fieldName, value);
        }
        std::list< std::pair<off_t, size_t> > blocks;
        for(const auto& block : resultBlocks.first->second) {
            blocks.push_back(std::pair<off_t, size_t>(block.lower(), block.upper() - block.lower()));
        }
        newEvent->setBlocks(blocks);
        newEvent->setStringProperty(FILE_PATH, path);

        m_blocks.erase(path);
        m_counters.erase(path);

        return newEvent;
    }

    return std::shared_ptr<Event>();
}

std::shared_ptr<Event> EventAggregator::ActualEventAggregator::getEventByProperty(const string & path, const string & value, const string & fieldName, const string & sumFieldName)
{
    auto counter = m_counters[path];
    auto blocks = m_blocks[path];

    auto newEvent = std::make_shared<Event>();
    newEvent->setStringProperty(SUM_FIELD_NAME, sumFieldName);
    newEvent->setNumericProperty(sumFieldName, counter);

    newEvent->setStringProperty(fieldName, value);
    std::list< std::pair<off_t, size_t> > newBlocks;
    for(const auto& block : blocks)
    {
        newBlocks.push_back(std::pair<off_t, size_t>(block.lower(), block.upper() - block.lower()));
    }

    newEvent->setBlocks(newBlocks);
    newEvent->setStringProperty(FILE_PATH, path);

    return newEvent;
}

std::list<std::shared_ptr<Event> > EventAggregator::ActualEventAggregator::getPendingEvents(const string & value, const string & fieldName, const string & sumFieldName)
{
    std::lock_guard<std::mutex> guard{m_aggregatorStateMutex};
    std::list<std::shared_ptr<Event> > processedEvents;
    for(auto & entry : m_counters)
    {
         auto processedEvent = getEventByProperty(entry.first, value, fieldName, sumFieldName);
         if(processedEvent)
             processedEvents.push_back(processedEvent);
    }
    m_blocks.clear();
    m_counters.clear();
    return processedEvents;
}

std::list<std::shared_ptr<Event> > EventAggregator::getPendingEvents(std::list<std::shared_ptr<Event> > events)
{
    std::list<std::shared_ptr<Event> > processedEvents;
    for(auto & event : events)
    {
        auto processedEvent = actualProcessEvent(event);
        if(processedEvent)
            processedEvents.push_back(processedEvent);
    }

    for(auto & entry : m_substreams)
    {
        auto result = entry.second.getPendingEvents(entry.first, m_fieldName, m_sumFieldName);
        processedEvents.insert(processedEvents.end(), result.begin(), result.end());
    }

    return processedEvents;
}

void EventAggregator::ActualEventAggregator::resetState()
{
    m_counters.clear();
    m_blocks.clear();
}
