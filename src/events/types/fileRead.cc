/**
 * @file fileRead.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRead.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRead::FileRead(std::string fileUuid, off_t offset, size_t size,
    std::string storageId, std::string fileId)
    : m_fileUuid{std::move(fileUuid)}
    , m_size{size}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size),
          FileBlock{std::move(storageId), std::move(fileId)}}}
{
}

StreamKey FileRead::streamKey() const { return StreamKey::FILE_READ; }

const std::string &FileRead::aggregationKey() const { return m_fileUuid; }

std::string FileRead::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRead', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

void FileRead::aggregate(EventPtr<FileRead> event)
{
    m_counter += event->m_counter;
    m_size += event->m_size;
    m_blocks += event->m_blocks;
}

ProtoEventPtr FileRead::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtoEvent>();
    auto fileReadMsg = msg->mutable_file_read();
    fileReadMsg->set_counter(m_counter);
    fileReadMsg->mutable_file_uuid()->swap(m_fileUuid);
    fileReadMsg->set_size(m_size);
    for (auto &block : m_blocks) {
        auto blockMsg = fileReadMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(block.first.upper() - block.first.lower());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
    }

    return msg;
}

} // namespace events
} // namespace client
} // namespace one
