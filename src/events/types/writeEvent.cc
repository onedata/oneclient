/**
 * @file writeEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/writeEvent.h"

#include "events/eventStream.h"
#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent()
    : Event{0}
{
}

WriteEvent::WriteEvent(off_t offset_, std::size_t size_, off_t fileSize_,
    std::string fileUuid_, std::size_t counter_, std::string storageId_,
    std::string fileId_)
    : Event{counter_}
    , m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_fileSize{fileSize_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

WriteEvent::WriteEvent(off_t offset_, std::size_t size_, std::string fileUuid_,
    std::size_t counter_, std::string storageId_, std::string fileId_)
    : Event{counter_}
    , m_fileUuid{std::move(fileUuid_)}
    , m_size{size_}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset_, offset_ + size_),
          FileBlock{std::move(storageId_), std::move(fileId_)}}}
{
}

const std::string &WriteEvent::fileUuid() const { return m_fileUuid; }

size_t WriteEvent::size() const { return m_size; }

boost::optional<off_t> WriteEvent::fileSize() const { return m_fileSize; }

bool operator==(const WriteEvent &lhs, const WriteEvent &rhs)
{
    return lhs.fileUuid() == rhs.fileUuid() && lhs.size() == rhs.size() &&
        lhs.fileSize() == rhs.fileSize() && lhs.blocks() == rhs.blocks();
}

WriteEvent &WriteEvent::operator+=(const WriteEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;

    if (event.m_fileSize)
        m_fileSize = event.m_fileSize;
    else if (m_fileSize &&
        m_fileSize.get() < boost::icl::last(event.m_blocks) + 1)
        m_fileSize = boost::icl::last(event.m_blocks) + 1;

    if (m_fileUuid.empty())
        m_fileUuid = event.m_fileUuid;

    m_blocks += event.m_blocks;
    return *this;
}

std::string WriteEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'WriteEvent', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

std::unique_ptr<one::messages::ProtocolClientMessage>
WriteEvent::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<one::messages::ProtocolClientMessage>();
    auto eventMsg = clientMsg->mutable_event();
    auto writeEventMsg = eventMsg->mutable_write_event();
    writeEventMsg->set_counter(m_counter);
    writeEventMsg->mutable_file_id()->swap(m_fileUuid);

    if (m_fileSize)
        writeEventMsg->set_file_size(m_fileSize.get());

    writeEventMsg->set_size(m_size);
    for (auto &block : m_blocks) {
        auto blockMsg = writeEventMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(boost::icl::size(block.first));
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
    }

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
