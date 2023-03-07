/**
 * @file reportFileWritten.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_REPORT_FILE_WRITTEN_H
#define ONECLIENT_MESSAGES_FUSE_REPORT_FILE_WRITTEN_H

#include "fileRequest.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c Truncate class represents a FUSE request for file truncation.
 */
class ReportFileWritten : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file.
     * @param offset Offset of the written bytes.
     * @param size The number of written bytes.
     */
    ReportFileWritten(std::string uuid, const size_t offset, const size_t size);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    size_t m_offset;
    size_t m_size;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_REPORT_FILE_WRITTEN_H
