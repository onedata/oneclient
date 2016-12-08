/**
 * @file fileTruncated.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_TRUNCATED_H
#define ONECLIENT_EVENTS_TYPES_FILE_TRUNCATED_H

#include "fileWritten.h"

namespace one {
namespace client {
namespace events {

/**
 * @c FileTruncated class represents a truncate file operation in the system.
 */
class FileTruncated : public FileWritten {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with the truncate operation.
     * @param fileSize Size of a file after the truncate operation.
     */
    FileTruncated(std::string fileUuid, off_t fileSize);
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_TRUNCATED_H
