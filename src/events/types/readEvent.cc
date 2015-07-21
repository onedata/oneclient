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

ReadEvent::ReadEvent(
    std::string fileId_, off_t offset_, size_t size_, std::size_t counter_)
    : Event{counter_}
    , m_fileId{std::move(fileId_)}
    , m_size{size_}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(
          offset_, offset_ + size_)}
{
}

const std::string &ReadEvent::fileId() const { return m_fileId; }

size_t ReadEvent::size() const { return m_size; }

const boost::icl::interval_set<off_t> &ReadEvent::blocks() const
{
    return m_blocks;
}

bool operator==(const ReadEvent &lhs, const ReadEvent &rhs)
{
    return lhs.fileId() == rhs.fileId() && lhs.size() == rhs.size() &&
        lhs.blocks() == rhs.blocks();
}

ReadEvent &ReadEvent::operator+=(const ReadEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    return *this;
}

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEvent', counter: " << m_counter << ", file ID: '"
           << m_fileId << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
ReadEvent::serialize() const
{
    auto clientMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto eventMsg = clientMsg->mutable_event();
    auto readEventMsg = eventMsg->mutable_read_event();
    readEventMsg->set_counter(m_counter);
    readEventMsg->set_file_id(m_fileId);
    readEventMsg->set_size(m_size);
    for (const auto &block : m_blocks) {
        auto blockMsg = readEventMsg->add_blocks();
        blockMsg->set_offset(block.lower());
        blockMsg->set_size(block.upper() - block.lower());
    }

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
