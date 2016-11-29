/**
 * @file fileRemovedEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemovedEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemovedEvent::FileRemovedEvent(const ProtocolMessage &msg)
    : m_fileUuid{msg.file_uuid()}
    , m_routingKey{"FileRemovedEventStream." + m_fileUuid}
{
}

const std::string &FileRemovedEvent::routingKey() const { return m_routingKey; }

const std::string &FileRemovedEvent::aggregationKey() const
{
    return m_fileUuid;
}

const std::string &FileRemovedEvent::fileUuid() const { return m_fileUuid; }

std::string FileRemovedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRemoved', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

void FileRemovedEvent::aggregate(ConstEventPtr event) {}

EventPtr FileRemovedEvent::clone() const
{
    return std::make_shared<FileRemovedEvent>(*this);
}

} // namespace events
} // namespace client
} // namespace one
