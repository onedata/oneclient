/**
 * @file verifyStorageTestFile.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "verifyStorageTestFile.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

VerifyStorageTestFile::VerifyStorageTestFile(std::string storageId,
    std::string spaceId, std::string fileId, std::string fileContent)
    : m_storageId{std::move(storageId)}
    , m_spaceId{std::move(spaceId)}
    , m_fileId{std::move(fileId)}
    , m_fileContent{std::move(fileContent)}
{
}

std::string VerifyStorageTestFile::toString() const
{
    std::stringstream ss;
    ss << "type: 'VerifyStorageTestFile', storage ID: '" << m_storageId
       << "', space ID: '" << m_spaceId << "', file ID: '" << m_fileId
       << "', file content: '" << m_fileContent << "'";
    return ss.str();
}

std::unique_ptr<ProtocolClientMessage>
VerifyStorageTestFile::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto fuseRequest = clientMsg->mutable_fuse_request();
    auto msg = fuseRequest->mutable_verify_storage_test_file();
    msg->mutable_storage_id()->swap(m_storageId);
    msg->mutable_space_id()->swap(m_spaceId);
    msg->mutable_file_id()->swap(m_fileId);
    msg->mutable_file_content()->swap(m_fileContent);

    return clientMsg;
}

} // namespace fuse
} // namespace messages
} // namespace one
