/**
 * @file fileAccessedEvent.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileAccessedEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileAccessedEvent::FileAccessedEvent(
    std::string fileUuid, uint64_t openCount, uint64_t releaseCount)
    : m_fileUuid{fileUuid}
    , m_openCount{openCount}
    , m_releaseCount{releaseCount}
{
}

const FileAccessedEvent::Key FileAccessedEvent::key() const
{
    return m_fileUuid;
}

const std::string &FileAccessedEvent::fileUuid() const { return m_fileUuid; }

const uint64_t &FileAccessedEvent::openCount() const { return m_openCount; }

const uint64_t &FileAccessedEvent::releaseCount() const
{
    return m_releaseCount;
}

void FileAccessedEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
    m_openCount += event->m_openCount;
    m_releaseCount += event->m_releaseCount;
}

std::string FileAccessedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAccessedEvent', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid << ", openCount: " << m_openCount
           << ", releaseCount: " << m_releaseCount;

    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> FileAccessedEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto FileAccessedEventMsg = eventMsg->mutable_file_accessed_event();
    eventMsg->set_counter(m_counter);
    FileAccessedEventMsg->mutable_file_uuid()->swap(m_fileUuid);
    FileAccessedEventMsg->set_open_count(m_openCount);
    FileAccessedEventMsg->set_release_count(m_releaseCount);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
