/**
 * @file fsync.h
 * @author Tomasz Lichon
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FSYNC_H
#define ONECLIENT_MESSAGES_FUSE_FSYNC_H

#include "fileRequest.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c FSync class represents a FUSE request for file fsync.
 */
class FSync : public FileRequest {
public:
    /**
     * Constructor.
     * @param dataOnly Flag indicating if only data (without metadata) should be
     * fsynced.
     * @param handleId Id of the handle to release.
     */
    FSync(std::string uuid, bool dataOnly, std::string handleId);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    bool m_dataOnly;
    std::string m_handleId;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FSYNC_H
