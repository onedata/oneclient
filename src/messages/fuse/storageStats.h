/**
 * @file storageStats.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <memory>
#include <string>

namespace one {
namespace clproto {
class StorageStats;
}
namespace messages {
namespace fuse {

/**
 * @c StorageStats provides information about storage capacity within a specific
 * space context.
 */
class StorageStats {
public:
    using ProtocolMessage = clproto::StorageStats;
    StorageStats() = default;

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c StorageStats
     * counterpart.
     */
    StorageStats(const ProtocolMessage &message);

    /**
     * @return Storage id.
     */
    const std::string &storageId() const { return m_storageId; }

    /**
     * @return Total storage capacity in bytes within the context a specific
     * space.
     */
    std::size_t size() const { return m_size; }

    /**
     * @return Stored data size on storage in bytes within the context of a
     * specific space.
     */
    std::size_t occupied() const { return m_occupied; }

    std::size_t free() const
    {
        if (m_occupied > m_size)
            return 0;

        return m_size - m_occupied;
    }

    std::string toString() const;

    void fillProtocolMessage(ProtocolMessage &message);

private:
    std::string m_storageId;
    std::size_t m_size;
    std::size_t m_occupied;
};

} // namespace fuse
} // namespace messages
} // namespace one
