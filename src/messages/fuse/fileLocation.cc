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

    deserialize(serverMessage->fuse_response().file_location());
}

FileLocation::FileLocation(const ProtocolMessage &message)
{
    deserialize(message);
}

const FileLocation::Key &FileLocation::key() const { return m_uuid; }

const std::string &FileLocation::uuid() const { return m_uuid; }

const std::string &FileLocation::storageId() const { return m_storageId; }

void FileLocation::storageId(std::string storageId_)
{
    m_storageId.swap(storageId_);
}

const std::string &FileLocation::fileId() const { return m_fileId; }

void FileLocation::fileId(std::string fileId_) { m_fileId.swap(fileId_); }

FileLocation::FileBlocksMap &FileLocation::blocks() { return m_blocks; }

const FileLocation::FileBlocksMap &FileLocation::blocks() const
{
    return m_blocks;
}

void FileLocation::aggregate(FileLocationPtr fileLocation)
{
    m_storageId.swap(fileLocation->m_storageId);
    m_fileId.swap(fileLocation->m_fileId);
    m_blocks.swap(fileLocation->m_blocks);
}

std::string FileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocation', uuid: '" << m_uuid << "', storageId: '"
           << m_storageId << "', fileId: '" << m_fileId << "', blocks: [";

    for (const auto &block : m_blocks)
        stream << block.first << " -> (" << block.second.storageId() << ", "
               << block.second.fileId() << "), ";

    stream << "]";
    return stream.str();
}

void FileLocation::deserialize(const ProtocolMessage &message)
{
    m_uuid = message.uuid();
    m_storageId = message.storage_id();
    m_fileId = message.file_id();

    for (const auto block : message.blocks()) {
        auto interval = boost::icl::discrete_interval<off_t>::right_open(
            block.offset(), block.offset() + block.size());

        auto &fileId_ =
            block.has_file_id() ? block.file_id() : message.file_id();

        auto &storageId_ =
            block.has_storage_id() ? block.storage_id() : message.storage_id();

        m_blocks += std::make_pair(interval, FileBlock{storageId_, fileId_});
    }
}

} // namespace fuse
} // namespace messages
} // namespace one
