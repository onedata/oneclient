/**
* @file writeEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events/types/writeEvent.h"
#include "messages/client/writeEventSerializer.h"

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent() { m_counter = 0; }

WriteEvent::WriteEvent(std::string fileId, off_t offset, size_t size,
                       off_t fileSize)
    : m_fileId{std::move(fileId)}
    , m_size{size}
    , m_fileSize{fileSize}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
{
}

WriteEvent &WriteEvent::operator+=(const WriteEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_fileSize = event.m_fileSize;
    m_blocks += event.m_blocks;
    m_blocks &= boost::icl::discrete_interval<off_t>::right_open(0, m_fileSize);
    return *this;
}

const std::string &WriteEvent::fileId() const { return m_fileId; }

std::unique_ptr<ClientMessageSerializer> WriteEvent::createSerializer() const
{
    return std::make_unique<one::client::WriteEventSerializer>();
}

std::ostream &operator<<(std::ostream &ostream, const WriteEvent &event)
{
    return ostream << "type: WRITE, counter: " << event.m_counter
                   << ", file ID: '" << event.m_fileId
                   << "', file size: " << event.m_fileSize
                   << ", size: " << event.m_size
                   << ", blocks: " << event.m_blocks;
}

} // namespace events
} // namespace client
} // namespace one
