/**
 * @file event.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/event.h"

#include "communication_protocol.pb.h"
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>
#include <map>

using namespace veil::client;
using namespace veil::client::events;
using namespace std;
using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

std::shared_ptr<Event> Event::createMkdirEvent(const string & filePath)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = "mkdir_event";
    event->m_stringProperties["filePath"] = filePath;
    return std::move(event);
}

std::shared_ptr<Event> Event::createWriteEvent(const string & filePath, long long bytes)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("write_event");
    event->m_stringProperties["filePath"] = filePath;
    event->m_numericProperties["bytes"] = bytes;
    return std::move(event);
}

std::shared_ptr<Event> Event::createReadEvent(const string & filePath, long long bytes)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("read_event");
    event->m_stringProperties["filePath"] = filePath;
    event->m_numericProperties["bytes"] = bytes;
    return std::move(event);
}

std::shared_ptr<Event> Event::createRmEvent(const string & filePath)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("rm_event");
    event->m_stringProperties["filePath"] = filePath;
    return std::move(event);
}

std::shared_ptr<Event> Event::createTruncateEvent(const string & filePath, off_t newSize){
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = "truncate_event";
    event->m_stringProperties["filePath"] = filePath;
    event->m_stringProperties["newSize"] = "newSize";
    return std::move(event);
}

std::shared_ptr<EventMessage> Event::createProtoMessage()
{
    auto eventMessage = std::make_shared<EventMessage>();
    for(map<string, string>::iterator it = m_stringProperties.begin(); it != m_stringProperties.end(); ++it){
        eventMessage->add_string_properties_keys(it->first);
        eventMessage->add_string_properties_values(it->second);
    }

    for(map<string, NumericProperty>::iterator it = m_numericProperties.begin(); it != m_numericProperties.end(); ++it){
        eventMessage->add_numeric_properties_keys(it->first);
        eventMessage->add_numeric_properties_values(it->second);
    }

    return std::move(eventMessage);
}

NumericProperty Event::getNumericProperty(const string & key, const NumericProperty defaultValue) const {
    map<string, NumericProperty>::const_iterator it = m_numericProperties.find(key);
    if(it == m_numericProperties.end()){
        return defaultValue;
    }else{
        return it->second;
    }
}

void Event::setNumericProperty(const string & key, NumericProperty value){
    m_numericProperties[key] = value;
}

int Event::getNumericPropertiesSize() const {
    return m_numericProperties.size();
}

string Event::getStringProperty(const string & key, const string & defaultValue) const {
    map<string, string>::const_iterator it = m_stringProperties.find(key);
    if(it == m_stringProperties.end()){
        return defaultValue;
    }else{
        return it->second;
    }
}

void Event::setStringProperty(const string & key, const string &value){
    m_stringProperties[key] = value;
}

int Event::getStringPropertiesSize() const {
    return m_stringProperties.size();
}

Event::Event(){}

Event::Event(const Event & anotherEvent)
{
    m_numericProperties = anotherEvent.m_numericProperties;
    m_stringProperties = anotherEvent.m_stringProperties;
}
