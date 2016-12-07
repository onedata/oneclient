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

class FileTruncated : public FileWritten {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param fileSize Size of a file after write operation.
     * occurred.
     */
    FileTruncated(std::string fileUuid, off_t fileSize);

    std::string toString() const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_TRUNCATED_H
