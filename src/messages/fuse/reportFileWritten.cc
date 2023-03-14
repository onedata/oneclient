/**
 * @file reportFileWritten.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "reportFileWritten.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ReportFileWritten::ReportFileWritten(
    std::string uuid, const size_t offset, const size_t size)
    : FileRequest{std::move(uuid)}
    , m_offset{offset}
    , m_size{size}
{
}

std::string ReportFileWritten::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReportFileWritten', uuid: " << m_contextGuid
           << "', offset: " << m_offset << "', size: " << m_size;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> ReportFileWritten::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto *cm = msg->mutable_fuse_request()
                   ->mutable_file_request()
                   ->mutable_file_written_range();

    cm->set_offset(m_offset);

    cm->set_size(m_size);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
