/**
 * @file fsStats.cc
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsStats.h"

#include "messages.pb.h"

#include <folly/Range.h>

#include <algorithm>
#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FSStats::FSStats(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_fs_stats())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "fs_stats field missing in FSStats message"};

    m_spaceId =
        serverMessage->mutable_fuse_response()->mutable_fs_stats()->space_id();

    auto storageStats = serverMessage->mutable_fuse_response()
                            ->mutable_fs_stats()
                            ->mutable_storage_stats();

    for (const auto &storage : folly::range(
             storageStats->pointer_begin(), storageStats->pointer_end())) {
        m_storageStats.emplace_back(*storage);
    }
}

std::string FSStats::toString() const
{
    std::stringstream stream;
    stream << "type: 'FSStats', spaceId: " << m_spaceId << " storageStats : [";

    for (const auto &storage : m_storageStats)
        stream << "{ storageId: " << storage.storageId()
               << ", size: " << storage.size()
               << ", occupied: " << storage.occupied() << "}";

    stream << "]";

    return stream.str();
}

std::size_t FSStats::getTotalSize() const
{
    std::size_t size = 0;
    for (const auto &storage : m_storageStats)
        size += storage.size();

    return size;
}

std::size_t FSStats::getTotalFreeSize() const
{
    std::size_t size = 0;
    for (const auto &storage : m_storageStats)
        size += storage.free();

    return size;
}
} // namespace fuse
} // namespace messages
} // namespace one
