/**
 * @file fileRenamedEntry.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRenamedEntry.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

FileRenamedEntry::FileRenamedEntry(const ProtocolMessage &message)
    : m_oldUuid{message.old_uuid()}
    , m_newUuid{message.new_uuid()}
    , m_newParentUuid{message.new_parent_uuid()}
    , m_newName{message.new_name()}
{
}

FileRenamedEntry::FileRenamedEntry(ProtocolMessage &message)
{
    fillProtocolMessage(message);
}

std::string FileRenamedEntry::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildren', oldUuid: '" << m_oldUuid << "', newUuid: '"
           << m_newUuid << "', newParentUuid: '" << m_newParentUuid
           << "', newName: '" << m_newName << "'";

    return stream.str();
}

void FileRenamedEntry::fillProtocolMessage(ProtocolMessage &message)
{
    message.mutable_old_uuid()->swap(m_oldUuid);
    message.mutable_new_uuid()->swap(m_newUuid);
    message.mutable_new_parent_uuid()->swap(m_newParentUuid);
    message.mutable_new_name()->swap(m_newName);
}

} // namespace fuse
} // namespace messages
} // namespace one
