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
    std::string spaceUuid, std::string fileId, std::string fileContent)
    : m_storageId{std::move(storageId)}
    , m_spaceUuid{std::move(spaceUuid)}
    , m_fileId{std::move(fileId)}
    , m_fileContent{std::move(fileContent)}
{
}

std::string VerifyStorageTestFile::toString() const
{
    std::stringstream ss;
    ss << "type: 'VerifyStorageTestFile', storage ID: '" << m_storageId
       << "', space UUID: '" << m_spaceUuid << "', file ID: '" << m_fileId
       << "', file content: '" << m_fileContent << "'";
    return ss.str();
}

std::unique_ptr<ProtocolClientMessage>
VerifyStorageTestFile::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto fuseRequest = clientMsg->mutable_fuse_request();
    auto msg = fuseRequest->mutable_verify_storage_test_file();
    msg->set_storage_id(m_storageId);
    msg->set_space_uuid(m_spaceUuid);
    msg->set_file_id(m_fileId);
    msg->set_file_content(m_fileContent);

    return clientMsg;
}

} // namespace fuse
} // namespace messages
} // namespace one
