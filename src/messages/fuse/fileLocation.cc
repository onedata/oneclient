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

using namespace std::literals;

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

const std::string &FileLocation::routingKey() const { return m_uuid; }

const std::string &FileLocation::aggregationKey() const { return m_uuid; }

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

void FileLocation::aggregate(client::events::ConstEventPtr event)
{
    auto fileLocationEvent = client::events::get<FileLocation>(event);
    m_storageId = fileLocationEvent->m_storageId;
    m_fileId = fileLocationEvent->m_fileId;
    m_blocks = fileLocationEvent->m_blocks;
}

client::events::EventPtr FileLocation::clone() const
{
    return std::make_shared<FileLocation>(*this);
}

void FileLocation::deserialize(const ProtocolMessage &message)
{
    m_uuid = message.uuid();
    m_spaceId = message.space_id();
    m_storageId = message.storage_id();
    m_fileId = message.file_id();
    std::string fileId_;
    std::string storageId_;

    for (const auto &block : message.blocks()) {
        auto interval = boost::icl::discrete_interval<off_t>::right_open(
            block.offset(), block.offset() + block.size());

        if (block.has_file_id())
            fileId_ = block.file_id();
        else
            fileId_ = m_fileId;

        if (block.has_storage_id())
            storageId_ = block.storage_id();
        else
            storageId_ = m_storageId;

        m_blocks += std::make_pair(
            interval, FileBlock{std::move(storageId_), std::move(fileId_)});
    }
}

} // namespace fuse
} // namespace messages
} // namespace one
