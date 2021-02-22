/**
 * @file makeSymLink.h
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_MAKE_SYMLINK_H
#define ONECLIENT_MESSAGES_FUSE_MAKE_SYMLINK_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The MakeSymLink class represents a FUSE request for creation of a symbolic link.
 */
class MakeSymLink : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the source file.
     * @param parentUuid UUID of the parent directory of target link.
     * @param name Name of the new hard link.
     */
    MakeSymLink(
        folly::fbstring parentUuid, folly::fbstring name, folly::fbstring link);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
    folly::fbstring m_link;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_MAKE_SYMLINK_H
