/**
 * @file storageStats.cc
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageStats.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

StorageStats::StorageStats(const ProtocolMessage &message)
    : m_storageId{message.storage_id()}
    , m_size{message.size()}
    , m_occupied{message.occupied()}
{
}

std::string StorageStats::toString() const
{
    std::stringstream stream;
    stream << "type: 'StorageStats', storageId: '" << m_storageId
           << "', size: '" << m_size << "', occupied: '" << m_occupied;

    return stream.str();
}

void StorageStats::fillProtocolMessage(ProtocolMessage &message)
{
    message.mutable_storage_id()->swap(m_storageId);
    message.set_size(m_size);
    message.set_occupied(m_occupied);
}

} // namespace fuse
} // namespace messages
} // namespace one
