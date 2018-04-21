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

SynchronizeBlock::SynchronizeBlock(
    std::string uuid, boost::icl::discrete_interval<off_t> block)
    : FileRequest{std::move(uuid)}
    , m_block{block}
{
}

std::string SynchronizeBlock::toString() const
{
    std::stringstream stream;
    stream << "type: 'SynchronizeBlock', uuid: " << m_contextGuid
           << ", block: " << m_block;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> SynchronizeBlock::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto sb = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_synchronize_block();

    sb->set_prefetch(true);
    sb->mutable_block()->set_offset(boost::icl::first(m_block));
    sb->mutable_block()->set_size(boost::icl::size(m_block));

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
