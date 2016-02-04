/**
 * @file createStorageTestFile.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "createStorageTestFile.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

CreateStorageTestFile::CreateStorageTestFile(
    std::string fileUuid, std::string storageId)
    : m_fileUuid{std::move(fileUuid)}
    , m_storageId{std::move(storageId)}
{
}

std::string CreateStorageTestFile::toString() const
{
    std::stringstream ss;
    ss << "type: 'CreateStorageTestFile', file UUID: '" << m_fileUuid
       << "', storage ID: '" << m_storageId << "'";
    return ss.str();
}

std::unique_ptr<ProtocolClientMessage>
CreateStorageTestFile::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto fuseRequest = clientMsg->mutable_fuse_request();
    auto msg = fuseRequest->mutable_create_storage_test_file();
    msg->set_file_uuid(m_fileUuid);
    msg->set_storage_id(m_storageId);

    return clientMsg;
}

} // namespace fuse
} // namespace messages
} // namespace one
