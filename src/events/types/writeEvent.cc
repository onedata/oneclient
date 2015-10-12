/**
 * @file writeEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/writeEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

const std::string WriteEvent::name = "WriteEvent";

WriteEvent::WriteEvent(
    std::string fileId_, off_t offset_, std::size_t size_, off_t fileSize_)
    : m_fileId{std::move(fileId_)}
    , m_size{size_}
    , m_fileSize{fileSize_}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(
          offset_, offset_ + size_)}
{
}

const std::string &WriteEvent::fileId() const { return m_fileId; }

size_t WriteEvent::size() const { return m_size; }

off_t WriteEvent::fileSize() const { return m_fileSize; }

const boost::icl::interval_set<off_t> &WriteEvent::blocks() const
{
    return m_blocks;
}

WriteEvent &WriteEvent::operator+=(const WriteEvent &evt)
{
    m_ctr += evt.m_ctr;
    m_size += evt.m_size;
    m_fileSize = evt.m_fileSize;
    m_blocks += evt.m_blocks;
    return *this;
}

bool WriteEvent::operator<(const WriteEvent &evt)
{
    return m_fileId < evt.m_fileId;
}

std::string WriteEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'WriteEvent', counter: " << m_ctr << ", file ID: '"
           << m_fileId << "', file size: " << m_fileSize << ", size: " << m_size
           << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
WriteEvent::serialize() const
{
    auto cliMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto evtMsg = cliMsg->mutable_event();
    auto writeEvtMsg = evtMsg->mutable_write_event();

    writeEvtMsg->set_counter(m_ctr);
    writeEvtMsg->set_file_id(m_fileId);
    writeEvtMsg->set_file_size(m_fileSize);
    writeEvtMsg->set_size(m_size);

    auto blocks_ = m_blocks;
    blocks_ &= boost::icl::discrete_interval<off_t>::right_open(0, m_fileSize);
    for (const auto &block : blocks_) {
        auto blockMsg = writeEvtMsg->add_blocks();
        blockMsg->set_offset(block.lower());
        blockMsg->set_size(block.upper() - block.lower());
    }

    return cliMsg;
}

} // namespace events
} // namespace client
} // namespace one
