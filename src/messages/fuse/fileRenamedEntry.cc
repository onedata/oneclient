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
    , m_newPath{message.new_path()}
{
}

FileRenamedEntry::FileRenamedEntry(ProtocolMessage &message)
{
    message.mutable_old_uuid()->swap(m_oldUuid);
    message.mutable_new_uuid()->swap(m_newUuid);
    message.mutable_new_path()->swap(m_newPath);
}

const std::string &FileRenamedEntry::oldUuid() const { return m_oldUuid; }

const std::string &FileRenamedEntry::newUuid() const { return m_newUuid; }

const std::string &FileRenamedEntry::newPath() const { return m_newPath; }

std::string FileRenamedEntry::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildren', oldUuid: '" << m_oldUuid << "', newUuid: '"
           << m_newUuid << "', newPath: '" << m_newPath << "'";

    return stream.str();
}

void FileRenamedEntry::fillProtocolMessage(ProtocolMessage &message)
{
    message.mutable_old_uuid()->swap(m_oldUuid);
    message.mutable_new_uuid()->swap(m_newUuid);
    message.mutable_new_path()->swap(m_newPath);
}

} // namespace fuse
} // namespace messages
} // namespace one
