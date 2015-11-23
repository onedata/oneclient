/**
 * @file fileLocation.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileLocation.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileLocation::FileLocation(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_location())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_location field missing"};

    auto fileLocation =
        serverMessage->mutable_fuse_response()->mutable_file_location();

    m_uuid.swap(*fileLocation->mutable_uuid());
    m_storageId.swap(*fileLocation->mutable_storage_id());
    m_fileId.swap(*fileLocation->mutable_file_id());

    for (auto &block : *fileLocation->mutable_blocks()) {
        auto interval = boost::icl::discrete_interval<off_t>::right_open(
            block.offset(), block.offset() + block.size());

        auto fileId_ = block.has_file_id() ? std::move(*block.mutable_file_id())
                                           : m_fileId;

        auto storageId_ = block.has_storage_id()
            ? std::move(*block.mutable_storage_id())
            : m_storageId;

        m_blocks += std::make_pair(
            interval, FileBlock{std::move(storageId_), std::move(fileId_)});
    }
}

std::string FileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocation', uuid: '" << m_uuid << "', blocks: [";

    for (const auto &block : m_blocks)
        stream << block.first << " -> (" << block.second.storageId() << ", "
               << block.second.fileId() << "), ";

    stream << "]";
    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
