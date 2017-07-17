/**
 * @file makeFile.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_MAKE_FILE_H
#define ONECLIENT_MESSAGES_FUSE_MAKE_FILE_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The MakeFile class represents a FUSE request for creation of a new node.
 */
class MakeFile : public FileRequest {
public:
    /**
     * Constructor.
     * @param parentUuid UUID of the directory in which to create the file.
     * @param name Name of the file to create.
     * @param mode Mode of the newly created file.
     */
    MakeFile(
        folly::fbstring parentUuid, folly::fbstring name, const mode_t mode);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
    mode_t m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_MAKE_FILE_H
