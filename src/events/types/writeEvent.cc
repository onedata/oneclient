/**
 * @file writeEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "writeEvent.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent(std::string fileUuid, off_t offset, size_t size,
    std::string storageId, std::string fileId, boost::optional<off_t> fileSize)
    : m_fileUuid{std::move(fileUuid)}
    , m_size{size}
    , m_blocks{{boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size),
          FileBlock{std::move(storageId), std::move(fileId)}}}
    , m_fileSize{std::move(fileSize)}
{
}

const std::string &WriteEvent::routingKey() const { return m_routingKey; }

const std::string &WriteEvent::aggregationKey() const { return m_fileUuid; }

std::string WriteEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'Write', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

void WriteEvent::aggregate(ConstEventPtr event)
{
    auto writeEvent = events::get<WriteEvent>(event);
    m_counter += writeEvent->m_counter;
    m_size += writeEvent->m_size;

    if (writeEvent->m_fileSize)
        m_fileSize = std::move(writeEvent->m_fileSize);
    else if (m_fileSize &&
        m_fileSize.get() < boost::icl::last(writeEvent->m_blocks) + 1)
        m_fileSize = boost::icl::last(writeEvent->m_blocks) + 1;

    m_blocks += writeEvent->m_blocks;
}

EventPtr WriteEvent::clone() const
{
    return std::make_shared<WriteEvent>(*this);
}

ProtoEventPtr WriteEvent::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtoEvent>();
    auto writeMsg = msg->mutable_write_event();

    writeMsg->set_counter(m_counter);
    writeMsg->mutable_file_uuid()->swap(m_fileUuid);
    writeMsg->set_size(m_size);

    auto blocks = m_blocks;
    if (m_fileSize) {
        writeMsg->set_file_size(m_fileSize.get());
        blocks &= boost::icl::discrete_interval<off_t>::right_open(
            0, m_fileSize.get());
    }

    for (auto &block : blocks) {
        auto blockMsg = writeMsg->add_blocks();
        blockMsg->set_offset(block.first.lower());
        blockMsg->set_size(boost::icl::size(block.first));
        blockMsg->mutable_storage_id()->swap(block.second.mutableStorageId());
        blockMsg->mutable_file_id()->swap(block.second.mutableFileId());
    }

    return msg;
}

} // namespace events
} // namespace client
} // namespace one
