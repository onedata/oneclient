/**
 * @file truncateEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/truncateEvent.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

const std::string TruncateEvent::name = "TruncateEvent";

TruncateEvent::TruncateEvent(std::string fileId_, off_t fileSize_)
    : WriteEvent{std::move(fileId_), 0, 0, fileSize_}
{
}

std::string TruncateEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'TruncateEvent', counter: " << m_ctr << ", file ID: '"
           << m_fileId << "', file size: " << m_fileSize;
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
