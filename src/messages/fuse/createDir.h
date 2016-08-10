/**
 * @file createDir.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CREATE_DIR_H
#define ONECLIENT_MESSAGES_FUSE_CREATE_DIR_H

#include "fileRequest.h"

#include <sys/types.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreateDir class represents a FUSE request for dir creation.
 */
class CreateDir : public FileRequest {
public:
    /**
     * Constructor.
     * @param parentUUID UUID of the parent directory.
     * @param name Name of the file to create.
     * @param mode File mode to create the file with.
     */
    CreateDir(std::string parentUUID, std::string name, mode_t mode);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_name;
    mode_t m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CREATE_DIR_H
