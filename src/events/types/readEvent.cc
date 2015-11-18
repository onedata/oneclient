/**
 * @file readEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/readEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent(off_t offset_, size_t size_, std::string fileUuid_,
    std::string storageId_, std::string fileId_)
    : m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

const ReadEvent::Key &ReadEvent::key() const { return m_fileUuid; }

const std::string &ReadEvent::fileUuid() const { return m_fileUuid; }

size_t ReadEvent::size() const { return m_size; }

const ReadEvent::FileBlocksMap &ReadEvent::blocks() const { return m_blocks; }

void ReadEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
    m_size += event->m_size;
    m_blocks += event->m_blocks;
}

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEvent', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> ReadEvent::serialize() const
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto readEventMsg = eventMsg->mutable_read_event();

    eventMsg->set_counter(m_counter);
    readEventMsg->set_file_uuid(m_fileUuid);
    readEventMsg->set_size(m_size);

    for (const auto &block : m_blocks) {
        auto blockMsg = readEventMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(block.first.upper() - block.first.lower());
        blockMsg->set_file_id(block.second.fileId());
        blockMsg->set_storage_id(block.second.storageId());
    }

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
