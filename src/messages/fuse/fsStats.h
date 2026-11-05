/**
 * @file fsStats.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "events/types/event.h"
#include "fuseResponse.h"
#include "storageStats.h"

#include "messages.pb.h"

#include <folly/FBString.h>
#include <folly/Optional.h>

#include <sys/types.h>

#include <chrono>
#include <cstdint>
#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FSStats class represents filesystem statistics for a specific space.
 */
class FSStats : public FuseResponse {
public:
    using ProtocolMessage = clproto::FSStats;

    /**
     * Constructor.
     * @param message Protocol Buffers message that wraps @c
     * one::clproto::FSStats message.
     */
    FSStats(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c FileAttr
     * counterpart.
     */
    FSStats(const ProtocolMessage &message);

    folly::fbstring spaceId() const { return m_spaceId; }

    const std::vector<StorageStats> &storageStats() const
    {
        return m_storageStats;
    }

    /**
     * Get total storage size.
     * @return Total storage size within a space.
     */
    std::size_t getTotalSize() const;

    /**
     * Get total free size
     * @return Total free storage size within a space.
     */
    std::size_t getTotalFreeSize() const;

    std::string toString() const override;

private:
    void deserialize(const ProtocolMessage &message);

    folly::fbstring m_spaceId;
    std::vector<StorageStats> m_storageStats;
};

} // namespace fuse
} // namespace messages
} // namespace one
