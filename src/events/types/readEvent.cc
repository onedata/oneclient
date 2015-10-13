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

ReadEvent::ReadEvent(off_t offset_, size_t size_, std::string fileUuid_,
    std::string storageId_, std::string fileId_)
    : m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

const std::string &ReadEvent::fileUuid() const { return m_fileUuid; }

size_t ReadEvent::size() const { return m_size; }

const ReadEvent::FileBlocksMap &ReadEvent::blocks() const { return m_blocks; }

ReadEvent &ReadEvent::operator+=(const ReadEvent &evt)
{
    m_ctr += evt.m_ctr;
    m_size += evt.m_size;
    m_blocks += evt.m_blocks;
    return *this;
}

bool ReadEvent::operator<(const ReadEvent &evt)
{
    return m_fileUuid < evt.m_fileUuid;
}

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReadEvent', counter: " << m_ctr << ", file UUID: '"
           << m_fileUuid << "', size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
ReadEvent::serialize() const
{
    auto cliMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto evtMsg = cliMsg->mutable_event();
    evtMsg->set_counter(m_ctr);
    auto readEvtMsg = evtMsg->mutable_read_event();
    readEvtMsg->set_file_uuid(m_fileUuid);
    readEvtMsg->set_size(m_size);
    for (const auto &block : m_blocks) {
        auto blockMsg = readEvtMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(block.first.upper() - block.first.lower());
        blockMsg->set_file_uuid(block.second.fileId());
        blockMsg->set_storage_id(block.second.storageId());
    }

    return cliMsg;
}

} // namespace events
} // namespace client
} // namespace one
