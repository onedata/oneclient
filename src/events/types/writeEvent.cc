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

const std::string WriteEvent::name = "WriteEvent";

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

const std::string &WriteEvent::fileUuid() const { return m_fileUuid; }

size_t WriteEvent::size() const { return m_size; }

boost::optional<off_t> WriteEvent::fileSize() const { return m_fileSize; }

const WriteEvent::FileBlocksMap &WriteEvent::blocks() const { return m_blocks; }

WriteEvent &WriteEvent::operator+=(const WriteEvent &evt)
{
    m_ctr += evt.m_ctr;
    m_size += evt.m_size;

    if (evt.m_fileSize)
        m_fileSize = evt.m_fileSize;
    else if (m_fileSize &&
        m_fileSize.get() < boost::icl::last(evt.m_blocks) + 1)
        m_fileSize = boost::icl::last(evt.m_blocks) + 1;

    m_blocks += evt.m_blocks;
    return *this;
}

bool WriteEvent::operator<(const WriteEvent &evt)
{
    return m_fileUuid < evt.m_fileUuid;
}

std::string WriteEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'WriteEvent', counter: " << m_ctr << ", file UUID: '"
           << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
WriteEvent::serialize() const
{
    auto cliMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto evtMsg = cliMsg->mutable_event();
    evtMsg->set_counter(m_ctr);
    auto writeEvtMsg = evtMsg->mutable_write_event();

    writeEvtMsg->set_file_uuid(m_fileUuid);
    writeEvtMsg->set_size(m_size);

    auto blocks_ = m_blocks;
    if (m_fileSize) {
        writeEvtMsg->set_file_size(m_fileSize.get());
        blocks_ &= boost::icl::discrete_interval<off_t>::right_open(
            0, m_fileSize.get());
    }

    for (const auto &block : blocks_) {
        auto blockMsg = writeEvtMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(boost::icl::size(block.first));
        blockMsg->set_storage_id(block.second.storageId());
        blockMsg->set_file_uuid(block.second.fileId());
    }

    return cliMsg;
}

} // namespace events
} // namespace client
} // namespace one
