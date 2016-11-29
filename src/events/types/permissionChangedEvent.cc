/**
 * @file permissionChangedEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "permissionChangedEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

PermissionChangedEvent::PermissionChangedEvent(const ProtocolMessage &msg)
    : m_fileUuid{msg.file_uuid()}
    , m_routingKey{"PermissionChangedEventStream." + m_fileUuid}
{
}

const std::string &PermissionChangedEvent::routingKey() const
{
    return m_routingKey;
}

const std::string &PermissionChangedEvent::aggregationKey() const
{
    return m_fileUuid;
}

const std::string &PermissionChangedEvent::fileUuid() const
{
    return m_fileUuid;
}

std::string PermissionChangedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'PermissionChanged', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

void PermissionChangedEvent::aggregate(ConstEventPtr event) {}

EventPtr PermissionChangedEvent::clone() const
{
    return std::make_shared<PermissionChangedEvent>(*this);
}

} // namespace events
} // namespace client
} // namespace one
