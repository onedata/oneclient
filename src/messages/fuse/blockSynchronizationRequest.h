/**
 * @file blockSynchronizationRequest.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fileRequest.h"
#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_map.hpp>

#include <string>
#include <sys/types.h>

namespace one {
namespace messages {
namespace fuse {

/**
 * The BlockSynchronizationRequest class represents a request for remote
 * synchronization of given range of file data, without awaiting a response.
 */
class BlockSynchronizationRequest : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to synchronize.
     * @param block interval that should be synchronized.
     */
    BlockSynchronizationRequest(std::string uuid,
        boost::icl::discrete_interval<off_t> block, int priority,
        bool prefetch = true);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    boost::icl::discrete_interval<off_t> m_block;
    int m_priority;
    bool m_prefetch;
};

} // namespace fuse
} // namespace messages
} // namespace one
