/**
 * @file storageTestFileVerification.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageTestFileVerification.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

StorageTestFileVerification::StorageTestFileVerification(
    const ProtocolMessage &message)
{
    m_storageId = message.storage_id();
}

const std::string &StorageTestFileVerification::storageId() const
{
    return m_storageId;
}

std::string StorageTestFileVerification::toString() const
{
    std::stringstream ss;
    ss << "type: 'StorageTestFileVerification', storage ID: '" << m_storageId
       << "'";
    return ss.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
