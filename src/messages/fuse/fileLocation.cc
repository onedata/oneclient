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

void FileLocation::putBlock(
    const off_t offset, const size_t size, FileBlock &&block)
{
    auto interval =
        boost::icl::discrete_interval<off_t>::right_open(offset, offset + size);
    m_blocks += std::make_pair(interval, block);
}

std::uint64_t FileLocation::version() const { return m_version; }

void FileLocation::version(std::uint64_t v) { m_version = v; }

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

std::string FileLocation::progressString(
    const size_t fileSize, const size_t progressSteps) const
{
    std::string result;
    result.reserve(progressSteps);

    assert(progressSteps > 0);

    if (fileSize < progressSteps * 2) {
        size_t intersectionLength = boost::icl::length(m_blocks &
            boost::icl::discrete_interval<off_t>::right_open(0, fileSize));

        if (intersectionLength == 0)
            result.append(progressSteps, ' ');
        else if (intersectionLength < fileSize / 2)
            result.append(progressSteps, '.');
        else if (intersectionLength < fileSize)
            result.append(progressSteps, 'o');
        else
            result.append(progressSteps, '#');
    }
    else {
        uint64_t progressStepByteLength = fileSize / progressSteps;

        for (size_t i = 0; i < progressSteps; i++) {
            auto startRange = i * progressStepByteLength;
            auto endRange = 0L;
            if (i < progressSteps - 1)
                endRange = (i + 1) * progressStepByteLength;
            else
                endRange = fileSize;

            size_t intersectionLength = boost::icl::length(m_blocks &
                boost::icl::discrete_interval<off_t>::right_open(
                    startRange, endRange));

            if (intersectionLength == 0)
                result += ' ';
            else if (intersectionLength <
                std::round(progressStepByteLength / 2.0))
                result += '.';
            else if (intersectionLength < progressStepByteLength)
                result += 'o';
            else
                result += '#';
        }
    }

    return result;
}

double FileLocation::replicationProgress(const size_t fileSize) const
{
    if (fileSize == 0)
        return 0.0;

    size_t intersectionLength = boost::icl::length(m_blocks &
        boost::icl::discrete_interval<off_t>::right_open(0, fileSize));

    return ((double)intersectionLength) / ((double)fileSize);
}

bool FileLocation::linearReadPrefetchThresholdReached(
    const double threshold, const size_t fileSize) const
{
    const off_t fileThresholdRange = fileSize * threshold;
    return boost::icl::length(m_blocks &
               boost::icl::discrete_interval<off_t>::right_open(0, fileSize)) >
        fileThresholdRange;
}

bool FileLocation::randomReadPrefetchThresholdReached(
    const double threshold, const size_t fileSize) const
{
    const off_t fileThresholdBytes = fileSize * threshold;

    return (boost::icl::interval_count(m_blocks) > 5) &&
        boost::icl::length(m_blocks &
            boost::icl::discrete_interval<off_t>::right_open(0, fileSize)) >
        fileThresholdBytes;
}

void FileLocation::deserialize(const ProtocolMessage &message)
{
    m_uuid = message.uuid();
    m_spaceId = message.space_id();
    m_storageId = message.storage_id();
    m_fileId = message.file_id();
    m_version = message.version();
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
