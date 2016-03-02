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

    deserialize(
        *serverMessage->mutable_fuse_response()->mutable_file_location());
}

FileLocation::FileLocation(ProtocolMessage message) { deserialize(message); }

const FileLocation::Key &FileLocation::key() const { return m_uuid; }

const std::string &FileLocation::uuid() const { return m_uuid; }

const std::string &FileLocation::spaceId() const { return m_spaceId; }

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

const boost::optional<std::string> &FileLocation::handleId() const
{
    return m_handleId;
}

void FileLocation::handleId(std::string handleId_)
{
    m_handleId.get().swap(handleId_);
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

    stream << "], handleId: ";

    if (m_handleId.is_initialized())
        stream << m_handleId.get();
    else
        stream << "unset";

    return stream.str();
}

void FileLocation::deserialize(ProtocolMessage &message)
{
    m_uuid.swap(*message.mutable_uuid());
    m_spaceId.swap(*message.mutable_space_id());
    m_storageId.swap(*message.mutable_storage_id());
    m_fileId.swap(*message.mutable_file_id());

    for (auto &block : *message.mutable_blocks()) {
        auto interval = boost::icl::discrete_interval<off_t>::right_open(
            block.offset(), block.offset() + block.size());

        std::string fileId_;
        if (block.has_file_id())
            fileId_.swap(*block.mutable_file_id());
        else
            fileId_ = m_fileId;

        std::string storageId_;
        if (block.has_storage_id())
            storageId_.swap(*block.mutable_storage_id());
        else
            storageId_ = m_storageId;

        m_blocks += std::make_pair(
            interval, FileBlock{std::move(storageId_), std::move(fileId_)});
    }

    if(message.has_handle_id()) {
        std::string handleId_;
        handleId_.swap(*message.mutable_handle_id());
        m_handleId = handleId_;
    }
}

} // namespace fuse
} // namespace messages
} // namespace one
