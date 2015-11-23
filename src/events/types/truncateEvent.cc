/**
 * @file truncateEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/types/truncateEvent.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

TruncateEvent::TruncateEvent(
    off_t fileSize_, std::string fileUuid_, std::size_t counter_)
    : WriteEvent{0, 0, fileSize_, std::move(fileUuid_), counter_}
{
}

std::string TruncateEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'TruncateEvent', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid << "', file size: " << m_fileSize;
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
