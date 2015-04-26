/**
* @file readEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events/eventStream.h"
#include "events/types/readEvent.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent() { m_counter = 0; }

ReadEvent::ReadEvent(std::weak_ptr<EventStream<ReadEvent>> eventStream,
                     std::string fileId, off_t offset, size_t size)
    : m_eventStream{std::move(eventStream)}
    , m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
{
}

void ReadEvent::emit() const
{
    auto eventStream = m_eventStream.lock();
    if (eventStream)
        eventStream->push(*this);
}

const std::string &ReadEvent::fileId() const { return m_fileId; }

size_t ReadEvent::size() const { return m_size; }

const boost::icl::interval_set<off_t> &ReadEvent::blocks() const
{
    return m_blocks;
}

ReadEvent &ReadEvent::operator+=(const ReadEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    return *this;
}

bool operator==(const ReadEvent &lhs, const ReadEvent &rhs)
{
    return lhs.fileId() == rhs.fileId() && lhs.size() == rhs.size() &&
           lhs.blocks() == rhs.blocks();
}

std::ostream &operator<<(std::ostream &ostream, const ReadEvent &event)
{
    return ostream << "type: READ, counter: " << event.m_counter
                   << ", file ID: '" << event.m_fileId
                   << ", size: " << event.m_size
                   << ", blocks: " << event.m_blocks;
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
