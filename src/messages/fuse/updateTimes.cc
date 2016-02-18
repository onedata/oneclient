/**
 * @file updateTimes.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "updateTimes.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

UpdateTimes::UpdateTimes(std::string uuid)
    : m_uuid{std::move(uuid)}
{
}

std::string UpdateTimes::toString() const
{
    std::stringstream stream;

    stream << "type: 'UpdateTimes', uuid: " << m_uuid;
    if (m_atime)
        stream << ", atime: " << std::chrono::system_clock::to_time_t(
                                     m_atime.get());
    if (m_ctime)
        stream << ", ctime: " << std::chrono::system_clock::to_time_t(
                                     m_ctime.get());
    if (m_mtime)
        stream << ", mtime: " << std::chrono::system_clock::to_time_t(
                                     m_mtime.get());

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> UpdateTimes::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto ut = msg->mutable_fuse_request()->mutable_update_times();

    ut->mutable_uuid()->swap(m_uuid);
    if (m_atime)
        ut->set_atime(std::chrono::system_clock::to_time_t(m_atime.get()));
    if (m_ctime)
        ut->set_ctime(std::chrono::system_clock::to_time_t(m_ctime.get()));
    if (m_mtime)
        ut->set_mtime(std::chrono::system_clock::to_time_t(m_mtime.get()));

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
