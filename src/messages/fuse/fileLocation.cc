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
{
    if (!serverMessage->fuse_response().has_file_location())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_location field missing"};

    const auto &fileLocation = serverMessage->fuse_response().file_location();
    m_uuid = fileLocation.uuid();
    m_storageId = fileLocation.storage_id();
    m_fileId = fileLocation.file_id();

    for (const auto block : fileLocation.blocks()) {
        auto interval = boost::icl::discrete_interval<off_t>::right_open(
            block.offset(), block.offset() + block.size());

        auto &fileId_ =
            block.has_file_id() ? block.file_id() : fileLocation.file_id();

        auto &storageId_ = block.has_storage_id() ? block.storage_id()
                                                  : fileLocation.storage_id();

        m_blocks += std::make_pair(interval, FileBlock{fileId_, storageId_});
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
