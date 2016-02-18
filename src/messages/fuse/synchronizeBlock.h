/**
 * @file synchronizeBlock.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_H
#define ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_H

#include "messages/clientMessage.h"
#include "messages/fuse/fileBlock.h"
#include <boost/icl/interval_map.hpp>

#include <sys/types.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The SynchronizeBlock class represents a request for remote synchronization of
 * given range of file data.
 */
class SynchronizeBlock : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to synchronize.
     * @param block interval that should be synchronized.
     */
    SynchronizeBlock(
        std::string uuid, boost::icl::discrete_interval<off_t> block);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
    boost::icl::discrete_interval<off_t> m_block;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_SYNCHRONIZE_BLOCK_H
