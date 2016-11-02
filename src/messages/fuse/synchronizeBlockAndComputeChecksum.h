/**
 * @file synchronizeBlock.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_AND_COMPUTE_CHECKSUM_H
#define ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_AND_COMPUTE_CHECKSUM_H

#include "fileRequest.h"
#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_map.hpp>

#include <string>
#include <sys/types.h>

namespace one {
namespace messages {
namespace fuse {

/**
 * The SynchronizeBlockAndComputeChecksum class represents a request for remote
 * synchronization of given range of file data, that returns data's md5 sum.
 */
class SynchronizeBlockAndComputeChecksum : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to synchronize.
     * @param block interval that should be synchronized.
     */
    SynchronizeBlockAndComputeChecksum(
        std::string uuid, boost::icl::discrete_interval<off_t> block);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    boost::icl::discrete_interval<off_t> m_block;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_AND_COMPUTE_CHECKSUM_H
