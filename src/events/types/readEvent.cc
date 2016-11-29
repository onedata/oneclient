/**
 * @file readEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "readEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent(std::string fileUuid, off_t offset, size_t size,
    std::string storageId, std::string fileId)
    : m_fileUuid{std::move(fileUuid)}
    , m_size{size}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size),
          FileBlock{std::move(storageId), std::move(fileId)}}}
{
}

const std::string &ReadEvent::routingKey() const { return m_routingKey; }

const std::string &ReadEvent::aggregationKey() const { return m_fileUuid; }

std::string ReadEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'Read', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

void ReadEvent::aggregate(ConstEventPtr event)
{
    auto readEvent = events::get<ReadEvent>(event);
    m_counter += readEvent->m_counter;
    m_size += readEvent->m_size;
    m_blocks += readEvent->m_blocks;
}

EventPtr ReadEvent::clone() const { return std::make_shared<ReadEvent>(*this); }

ProtoEventPtr ReadEvent::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtoEvent>();
    auto readMsg = msg->mutable_read_event();
    readMsg->set_counter(m_counter);
    readMsg->mutable_file_uuid()->swap(m_fileUuid);
    readMsg->set_size(m_size);
    for (auto &block : m_blocks) {
        auto blockMsg = readMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(block.first.upper() - block.first.lower());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
    }

    return msg;
}

} // namespace events
} // namespace client
} // namespace one
