/**
* @file readEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events/types/readEvent.h"
#include "messages/client/readEventSerializer.h"

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent() { m_counter = 0; }

ReadEvent::ReadEvent(std::string fileId, off_t offset, size_t size)
    : m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
{
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

std::unique_ptr<ClientMessageSerializer> ReadEvent::createSerializer() const
{
    return std::make_unique<one::client::ReadEventSerializer>();
}

} // namespace events
} // namespace client
} // namespace one
