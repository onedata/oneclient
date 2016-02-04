/**
 * @file storageTestFile.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageTestFile.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

StorageTestFile::StorageTestFile(const ProtocolMessage &message)
{
    m_helperParams = HelperParams{message.helper_params()};
    m_storageId = message.storage_id();
    m_spaceUuid = message.space_uuid();
    m_fileId = message.file_id();
    m_fileContent = message.file_content();
}

const HelperParams &StorageTestFile::helperParams() const
{
    return m_helperParams;
}

const std::string &StorageTestFile::storageId() const { return m_storageId; }

const std::string &StorageTestFile::spaceUuid() const { return m_spaceUuid; }

const std::string &StorageTestFile::fileId() const { return m_fileId; }

const std::string &StorageTestFile::fileContent() const
{
    return m_fileContent;
}

std::string StorageTestFile::toString() const
{
    std::stringstream ss;
    ss << "type: 'StorageTestFile', storage ID: '" << m_storageId
       << "', space UUID: '" << m_spaceUuid << "', file ID: '" << m_fileId
       << "', file content: '" << m_fileContent
       << "', helper parameters: " << m_helperParams.toString();
    return ss.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
