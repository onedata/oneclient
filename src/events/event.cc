/**
 * @file event.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/event.h"

#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"

#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>

#include <map>

using namespace std;
using namespace one::clproto::fuse_messages;
using namespace one::clproto::communication_protocol;

namespace one
{
namespace client
{
namespace events
{

std::shared_ptr<Event> Event::createMkdirEvent(const string & filePath)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = "mkdir_event";
    event->m_stringProperties["filePath"] = filePath;
    return event;
}

std::shared_ptr<Event> Event::createWriteEvent(const string& fileUUID, const string & filePath, off_t offset, size_t size)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("write_event");
    event->m_stringProperties["filePath"] = filePath;
    event->m_stringProperties["fileUUID"] = fileUUID;
    event->m_numericProperties["bytes"] = size;
    event->m_blocks = list< pair<off_t, size_t> > { std::pair<off_t, size_t>(offset, size) };
    return event;
}

std::shared_ptr<Event> Event::createReadEvent(const string& fileUUID, const string & filePath, off_t offset, size_t size)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("read_event");
    event->m_stringProperties["filePath"] = filePath;\
    event->m_stringProperties["fileUUID"] = fileUUID;
    event->m_numericProperties["bytes"] = size;
    event->m_blocks = list< pair<off_t, size_t> > { std::pair<off_t, size_t>(offset, size) };
    return event;
}

std::shared_ptr<Event> Event::createRmEvent(const string& fileUUID, const string & filePath)
{
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = string("rm_event");
    event->m_stringProperties["filePath"] = filePath;
    event->m_stringProperties["fileUUID"] = fileUUID;
    return event;
}

std::shared_ptr<Event> Event::createTruncateEvent(const string& fileUUID, const string & filePath, off_t newSize){
    auto event = std::make_shared<Event>();
    event->m_stringProperties["type"] = "truncate_event";
    event->m_stringProperties["filePath"] = filePath;
    event->m_stringProperties["fileUUID"] = fileUUID;
    event->m_numericProperties["newSize"] = newSize;
    return event;
}

std::shared_ptr<EventMessage> Event::createProtoMessage()
{
    auto eventMessage = std::make_shared<EventMessage>();
    for(const auto & elem : m_stringProperties){
        eventMessage->add_string_properties_keys(elem.first);
        eventMessage->add_string_properties_values(elem.second);
    }

    for(const auto & elem : m_numericProperties){
        eventMessage->add_numeric_properties_keys(elem.first);
        eventMessage->add_numeric_properties_values(elem.second);
    }

    for(const auto & block : m_blocks) {
        auto blockMessage = eventMessage->add_block();
        blockMessage->set_offset(block.first);
        blockMessage->set_size(block.second);
    }

    return eventMessage;
}

NumericProperty Event::getNumericProperty(const string & key, const NumericProperty defaultValue) const {
    auto it = m_numericProperties.find(key);
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
    auto it = m_stringProperties.find(key);
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

const std::list< std::pair<off_t, size_t> >& Event::getBlocks() const {
    return m_blocks;
}

void Event::setBlocks(const std::list< std::pair<off_t, size_t> >& blocks) {
    m_blocks = blocks;
}

Event::Event(const Event & anotherEvent)
{
    m_numericProperties = anotherEvent.m_numericProperties;
    m_stringProperties = anotherEvent.m_stringProperties;
    m_blocks = anotherEvent.m_blocks;
}

std::ostream& operator<<(std::ostream& os, const Event& obj)
{
    os << "event:\n";
    for(auto & entry : obj.m_stringProperties)
    {
        os << entry.first << " : " << entry.second << "\n";
    }
    for(auto & entry : obj.m_numericProperties)
    {
        os << entry.first << " : " << entry.second << "\n";
    }
    os << "blocks: \n";
    for(auto &block : obj.m_blocks)
    {
        os << block.first << " - " << block.second << "\n";
    }
    return os;
}

} // namespace events
} // namespace client
} // namespace one
