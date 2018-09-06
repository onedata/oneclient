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

    invalidateCachedValues();
}

FileLocation::FileLocation(const ProtocolMessage &message)
{
    deserialize(message);

    invalidateCachedValues();
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

const FileLocation::FileBlocksMap &FileLocation::blocks() const
{
    return m_blocks;
}

unsigned int FileLocation::blocksCount() const
{
    return boost::icl::interval_count(m_blocks);
}

unsigned int FileLocation::blocksInRange(
    const off_t start, const off_t end) const
{
    auto rangeIt = m_blocks.equal_range(
        boost::icl::discrete_interval<off_t>::right_open(start, end));

    unsigned int res = 0;
    for (auto it = rangeIt.first; it != rangeIt.second; ++it)
        res++;

    return res;
}

size_t FileLocation::blocksLengthInRange(
    const off_t start, const off_t end) const
{
    auto rangeIt = m_blocks.equal_range(
        boost::icl::discrete_interval<off_t>::right_open(start, end));

    size_t res = 0;
    for (auto it = rangeIt.first; it != rangeIt.second; ++it) {
        auto lower = std::max(start, it->first.lower());
        auto upper = std::min(it->first.upper(), end);
        res += (upper - lower);
    }

    return res;
}

void FileLocation::putBlock(
    const off_t offset, const size_t size, FileBlock &&block)
{
    auto interval =
        boost::icl::discrete_interval<off_t>::right_open(offset, offset + size);

    putBlock(std::make_pair<boost::icl::discrete_interval<off_t>, FileBlock>(
        std::move(interval), std::forward<FileBlock>(block)));
}

void FileLocation::putBlock(
    const std::pair<boost::icl::discrete_interval<off_t>, FileBlock> &block)
{
    m_blocks += block;

    invalidateCachedValues();
}

void FileLocation::truncate(const boost::icl::discrete_interval<off_t> &range)
{
    m_blocks &= range;

    invalidateCachedValues();
}

void FileLocation::update(const FileBlocksMap &blocks)
{
    m_blocks = blocks;

    invalidateCachedValues();
}

void FileLocation::updateInRange(
    const off_t start, const off_t end, const FileLocation &blocks)
{
    const auto updateRange =
        boost::icl::discrete_interval<off_t>::right_open(start, end);

    m_blocks.erase(updateRange);
    m_blocks += blocks.blocks() & updateRange;

    invalidateCachedValues();
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
    if (!m_progressStringCachedValid) {
        std::string result;
        result.reserve(progressSteps);

        assert(progressSteps > 0);

        if (fileSize < progressSteps * 2) {
            size_t intersectionLength =
                std::min<size_t>(boost::icl::length(m_blocks), fileSize);

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

                size_t intersectionLength =
                    blocksLengthInRange(startRange, endRange);

                if (intersectionLength == 0)
                    result += ' ';
                else if (intersectionLength < progressStepByteLength / 2)
                    result += '.';
                else if (intersectionLength < progressStepByteLength)
                    result += 'o';
                else
                    result += '#';
            }
        }

        m_progressStringCachedValue = result;
    }

    return m_progressStringCachedValue;
}

double FileLocation::replicationProgress(const size_t fileSize) const
{
    if (fileSize == 0)
        return 0.0;

    if (!m_replicationProgressCachedValid) {
        size_t intersectionLength =
            std::min<size_t>(boost::icl::length(m_blocks), fileSize);

        m_replicationProgressCachedValue =
            ((double)intersectionLength) / ((double)fileSize);

        m_replicationProgressCachedValid = true;
    }

    return m_replicationProgressCachedValue;
}

bool FileLocation::isReplicationComplete(const size_t fileSize) const
{
    if (fileSize == 0)
        return true;

    if (blocksCount() != 1)
        return false;

    return (size_t)(boost::icl::length(m_blocks)) >= fileSize;
}

bool FileLocation::linearReadPrefetchThresholdReached(
    const double threshold, const size_t fileSize) const
{
    const off_t fileThresholdRange = fileSize * threshold;
    return boost::icl::length(m_blocks) > fileThresholdRange;
}

bool FileLocation::randomReadPrefetchThresholdReached(
    const double threshold, const size_t fileSize) const
{
    const off_t fileThresholdBytes = fileSize * threshold;

    // If at least 5 different blocks are in the map and their overall size
    // is larger then threshold, return true
    return (blocksCount() > 5) &&
        boost::icl::length(m_blocks) > fileThresholdBytes;
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

void FileLocation::invalidateCachedValues()
{
    m_replicationProgressCachedValid = false;
    m_progressStringCachedValid = false;
}

} // namespace fuse
} // namespace messages
} // namespace one
