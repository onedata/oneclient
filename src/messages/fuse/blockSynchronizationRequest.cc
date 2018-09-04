/**
 * @file blockSynchronizationRequest.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "blockSynchronizationRequest.h"
#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

BlockSynchronizationRequest::BlockSynchronizationRequest(std::string uuid,
    boost::icl::discrete_interval<off_t> block, int priority, bool prefetch)
    : FileRequest{std::move(uuid)}
    , m_block{block}
    , m_priority{priority}
    , m_prefetch{prefetch}
{
}

std::string BlockSynchronizationRequest::toString() const
{
    std::stringstream stream;
    stream << "type: 'SynchronizeBlock', uuid: " << m_contextGuid
           << ", block: " << m_block << ", priority: " << m_priority
           << ", prefetch: " << m_prefetch;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
BlockSynchronizationRequest::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto sb = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_block_synchronization_request();

    sb->set_prefetch(m_prefetch);
    sb->set_priority(m_priority);
    sb->mutable_block()->set_offset(boost::icl::first(m_block));
    sb->mutable_block()->set_size(boost::icl::size(m_block));

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
