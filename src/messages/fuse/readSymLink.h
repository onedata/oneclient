/**
 * @file readSymLink.h
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_READ_SYMLINK_H
#define ONECLIENT_MESSAGES_FUSE_READ_SYMLINK_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The ReadSymLink class represents a FUSE request for reading target of a
 * symbolic link.
 */
class ReadSymLink : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the symbolic link.
     */
    ReadSymLink(folly::fbstring uuid);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_READ_SYMLINK_H
