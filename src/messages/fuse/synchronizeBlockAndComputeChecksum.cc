/**
 * @file synchronizeBlockAndComputeChecksum.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "synchronizeBlockAndComputeChecksum.h"
#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

SynchronizeBlockAndComputeChecksum::SynchronizeBlockAndComputeChecksum(
    std::string uuid, boost::icl::discrete_interval<off_t> block, int priority)
    : FileRequest{std::move(uuid)}
    , m_block{block}
    , m_priority{priority}
{
}

std::string SynchronizeBlockAndComputeChecksum::toString() const
{
    std::stringstream stream;
    stream << "type: 'SynchronizeBlockAndComputeChecksum', uuid: "
           << m_contextGuid << ", block: " << m_block
           << ", priority: " << m_priority;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
SynchronizeBlockAndComputeChecksum::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto sb = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_synchronize_block_and_compute_checksum();

    sb->mutable_block()->set_offset(boost::icl::first(m_block));
    sb->mutable_block()->set_size(boost::icl::size(m_block));
    sb->set_priority(m_priority);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
