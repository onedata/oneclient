/**
 * @file filePermChanged.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "filePermChanged.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FilePermChanged::FilePermChanged(const ProtocolMessage &msg)
    : m_fileUuid{msg.file_uuid()}
{
}

StreamKey FilePermChanged::streamKey() const
{
    return StreamKey::FILE_PERM_CHANGED;
}

const std::string &FilePermChanged::fileUuid() const { return m_fileUuid; }

std::string FilePermChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'PermissionChanged', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
