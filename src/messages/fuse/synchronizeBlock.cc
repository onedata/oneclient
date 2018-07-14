/**
 * @file synchronizeBlock.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "synchronizeBlock.h"
#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

SynchronizeBlock::SynchronizeBlock(std::string uuid,
    boost::icl::discrete_interval<off_t> block, int priority, bool prefetch)
    : FileRequest{std::move(uuid)}
    , m_block{block}
    , m_priority{priority}
    , m_prefetch{prefetch}
{
}

std::string SynchronizeBlock::toString() const
{
    std::stringstream stream;
    stream << "type: 'SynchronizeBlock', uuid: " << m_contextGuid
           << ", block: " << m_block << ", priority: " << m_priority
           << ", prefetch: " << m_prefetch;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> SynchronizeBlock::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto sb = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_synchronize_block();

    sb->set_prefetch(m_prefetch);
    sb->set_priority(m_priority);
    sb->mutable_block()->set_offset(boost::icl::first(m_block));
    sb->mutable_block()->set_size(boost::icl::size(m_block));

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
