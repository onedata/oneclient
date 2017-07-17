/**
 * @file createFile.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CREATE_FILE_H
#define ONECLIENT_MESSAGES_FUSE_CREATE_FILE_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreateFile class represents a FUSE request for creation of a new file.
 */
class CreateFile : public FileRequest {
public:
    /**
     * Constructor.
     * @param parentUuid UUID of the directory in which to create the file.
     * @param name Name of the file to create.
     * @param mode Mode of the newly created file.
     * @param flag Open flag.
     */
    CreateFile(folly::fbstring parentUuid, folly::fbstring name,
        const mode_t mode, const one::helpers::Flag flag);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
    mode_t m_mode;
    one::helpers::Flag m_flag;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CREATE_FILE_H
