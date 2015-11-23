/**
 * @file readEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/readEvent.h"

#include "events/eventStream.h"
#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent()
    : Event{0}
{
}

ReadEvent::ReadEvent(off_t offset_, size_t size_, std::string fileUuid_,
    std::size_t counter_, std::string storageId_, std::string fileId_)
    : Event{counter_}
    , m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

const std::string &ReadEvent::fileUuid() const { return m_fileUuid; }

size_t ReadEvent::size() const { return m_size; }

bool operator==(const ReadEvent &lhs, const ReadEvent &rhs)
{
    return lhs.fileUuid() == rhs.fileUuid() && lhs.size() == rhs.size() &&
        lhs.blocks() == rhs.blocks();
}

ReadEvent &ReadEvent::operator+=(const ReadEvent &event)
{
    if (m_fileUuid.empty())
        m_fileUuid = event.m_fileUuid;

    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    return *this;
}

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEvent', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
ReadEvent::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto eventMsg = clientMsg->mutable_event();
    auto readEventMsg = eventMsg->mutable_read_event();
    readEventMsg->set_counter(m_counter);
    readEventMsg->mutable_file_id()->swap(m_fileUuid);
    readEventMsg->set_size(m_size);
    for (auto &block : m_blocks) {
        auto blockMsg = readEventMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(block.first.upper() - block.first.lower());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
    }

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
