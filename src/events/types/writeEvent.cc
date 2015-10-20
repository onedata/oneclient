/**
 * @file writeEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/writeEvent.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent(off_t offset_, std::size_t size_, std::string fileUuid_,
    std::string storageId_, std::string fileId_)
    : m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

WriteEvent::WriteEvent(off_t offset_, std::size_t size_, off_t fileSize_,
    std::string fileUuid_, std::string storageId_, std::string fileId_)
    : m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_fileSize{fileSize_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

const WriteEvent::Key &WriteEvent::key() const { return m_fileUuid; }

const std::string &WriteEvent::fileUuid() const { return m_fileUuid; }

size_t WriteEvent::size() const { return m_size; }

boost::optional<off_t> WriteEvent::fileSize() const { return m_fileSize; }

const WriteEvent::FileBlocksMap &WriteEvent::blocks() const { return m_blocks; }

void WriteEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
    m_size += event->m_size;

    if (event->m_fileSize)
        m_fileSize = std::move(event->m_fileSize);
    else if (m_fileSize &&
        m_fileSize.get() < boost::icl::last(event->m_blocks) + 1)
        m_fileSize = boost::icl::last(event->m_blocks) + 1;

    m_blocks += event->m_blocks;
}

std::string WriteEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'WriteEvent', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage> WriteEvent::serialize() const
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto eventMsg = clientMsg->mutable_event();
    auto writeEventMsg = eventMsg->mutable_write_event();

    eventMsg->set_counter(m_counter);
    writeEventMsg->set_file_uuid(m_fileUuid);
    writeEventMsg->set_size(m_size);

    auto blocks_ = m_blocks;
    if (m_fileSize) {
        writeEventMsg->set_file_size(m_fileSize.get());
        blocks_ &= boost::icl::discrete_interval<off_t>::right_open(
            0, m_fileSize.get());
    }

    for (const auto &block : blocks_) {
        auto blockMsg = writeEventMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(boost::icl::size(block.first));
        blockMsg->set_storage_id(block.second.storageId());
        blockMsg->set_file_id(block.second.fileId());
    }

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
