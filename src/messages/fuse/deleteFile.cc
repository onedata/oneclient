/**
 * @file deleteFile.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "deleteFile.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

DeleteFile::DeleteFile(std::string uuid)
    : m_uuid{std::move(uuid)}
    , m_silent{false}
{
}

std::string DeleteFile::toString() const
{
    std::stringstream stream;
    stream << "type: 'DeleteFile', uuid: " << m_uuid
           << ", silent: " << m_silent;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> DeleteFile::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto df = msg->mutable_fuse_request()->mutable_delete_file();

    df->mutable_uuid()->swap(m_uuid);
    df->set_silent(m_silent);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
