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

StorageTestFile::StorageTestFile(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    if (!serverMessage->fuse_response().has_storage_test_file())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "storage_test_file field missing"};

    auto &message =
        *serverMessage->mutable_fuse_response()->mutable_storage_test_file();

    m_helperParams = HelperParams{*message.mutable_helper_params()};
    message.mutable_space_uuid()->swap(m_spaceUuid);
    message.mutable_file_id()->swap(m_fileId);
    message.mutable_file_content()->swap(m_fileContent);
}

const HelperParams &StorageTestFile::helperParams() const
{
    return m_helperParams;
}

const std::string &StorageTestFile::spaceUuid() const { return m_spaceUuid; }

const std::string &StorageTestFile::fileId() const { return m_fileId; }

const std::string &StorageTestFile::fileContent() const
{
    return m_fileContent;
}

std::string StorageTestFile::toString() const
{
    std::stringstream ss;
    ss << "type: 'StorageTestFile', space UUID: '" << m_spaceUuid
       << "', file ID: '" << m_fileId << "', file content: '" << m_fileContent
       << "', helper parameters: " << m_helperParams.toString();
    return ss.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
