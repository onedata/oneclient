/**
 * @file truncate.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/fuse/truncate.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Truncate::Truncate(std::string uuid, const off_t size)
    : m_uuid{std::move(uuid)}
    , m_size{size}
{
}

std::string Truncate::toString() const
{
    std::stringstream stream;
    stream << "type: 'Truncate', uuid: '" << m_uuid << "', size: " << m_size;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Truncate::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto cm = msg->mutable_fuse_request()->mutable_truncate();

    cm->set_uuid(m_uuid);
    cm->set_size(m_size);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
