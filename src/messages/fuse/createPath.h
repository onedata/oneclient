/**
 * @file createPath.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CREATE_PATH_H
#define ONECLIENT_MESSAGES_FUSE_CREATE_PATH_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreatePath class represents a FUSE request for creation of a new
 * directory path.
 */
class CreatePath : public FileRequest {
public:
    /**
     * Constructor.
     * @param parentUuid UUID of the directory in which to create the file.
     * @param name Name of the file to create.
     */
    CreatePath(const folly::fbstring &parentUuid, folly::fbstring path);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_path;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CREATE_PATH_H
