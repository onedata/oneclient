/**
 * @file fileTruncated.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileTruncated.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

FileTruncated::FileTruncated(std::string fileUuid, off_t fileSize)
    : FileWritten(std::move(fileUuid), 0, 0, "", "", fileSize)
{
}

std::string FileTruncated::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileTruncated', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid << "', file size: " << m_fileSize
           << ", size: " << m_size << ", blocks: " << m_blocks;
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
