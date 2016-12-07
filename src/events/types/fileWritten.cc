/**
 * @file fileWritten.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileWritten.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

FileWritten::FileWritten(std::string fileUuid, off_t offset, size_t size,
    std::string storageId, std::string fileId, boost::optional<off_t> fileSize)
    : m_fileUuid{std::move(fileUuid)}
    , m_size{size}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size),
          FileBlock{std::move(storageId), std::move(fileId)}}}
    , m_fileSize{std::move(fileSize)}
{
}

StreamKey FileWritten::streamKey() const { return StreamKey::FILE_WRITTEN; }

const std::string &FileWritten::aggregationKey() const { return m_fileUuid; }

std::string FileWritten::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileWritten', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

void FileWritten::aggregate(EventPtr<FileWritten> event)
{
    m_counter += event->m_counter;
    m_size += event->m_size;

    if (event->m_fileSize)
        m_fileSize = event->m_fileSize;
    else if (m_fileSize &&
        m_fileSize.get() < boost::icl::last(event->m_blocks) + 1)
        m_fileSize = boost::icl::last(event->m_blocks) + 1;

    m_blocks += event->m_blocks;
}

ProtoEventPtr FileWritten::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtoEvent>();
    auto fileWrittenMsg = msg->mutable_file_written();

    fileWrittenMsg->set_counter(m_counter);
    fileWrittenMsg->mutable_file_uuid()->swap(m_fileUuid);
    fileWrittenMsg->set_size(m_size);

    auto blocks = m_blocks;
    if (m_fileSize) {
        fileWrittenMsg->set_file_size(m_fileSize.get());
        blocks &= boost::icl::discrete_interval<off_t>::right_open(
            0, m_fileSize.get());
    }

    for (auto &block : blocks) {
        auto blockMsg = fileWrittenMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(boost::icl::size(block.first));
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
    }

    return msg;
}

} // namespace events
} // namespace client
} // namespace one
