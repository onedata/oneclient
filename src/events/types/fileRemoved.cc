/**
 * @file fileRemoved.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemoved.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemoved::FileRemoved(const ProtocolMessage &msg)
    : m_fileUuid{msg.file_uuid()}
{
}

StreamKey FileRemoved::streamKey() const { return StreamKey::FILE_REMOVED; }

const std::string &FileRemoved::fileUuid() const { return m_fileUuid; }

std::string FileRemoved::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRemoved', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
