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

const std::string ReadEvent::name = "ReadEvent";

ReadEvent::ReadEvent(std::string fileId_, off_t offset_, size_t size_)
    : m_fileId{std::move(fileId_)}
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

ReadEvent &ReadEvent::operator+=(const ReadEvent &evt)
{
    m_ctr += evt.m_ctr;
    m_size += evt.m_size;
    m_blocks += evt.m_blocks;
    return *this;
}

bool ReadEvent::operator<(const ReadEvent &evt)
{
    return m_fileId < evt.m_fileId;
}

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEvent', counter: " << m_ctr << ", file ID: '"
           << m_fileId << "', size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
ReadEvent::serialize() const
{
    auto cliMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto evtMsg = cliMsg->mutable_event();
    auto readEvtMsg = evtMsg->mutable_read_event();
    readEvtMsg->set_counter(m_ctr);
    readEvtMsg->set_file_id(m_fileId);
    readEvtMsg->set_size(m_size);
    for (const auto &block : m_blocks) {
        auto blockMsg = readEvtMsg->add_blocks();
        blockMsg->set_offset(block.lower());
        blockMsg->set_size(block.upper() - block.lower());
    }

    return cliMsg;
}

} // namespace events
} // namespace client
} // namespace one
