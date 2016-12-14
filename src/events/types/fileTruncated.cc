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

} // namespace events
} // namespace client
} // namespace one
