/**
 * @file openFile.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_OPEN_FILE_H
#define ONECLIENT_MESSAGES_FUSE_OPEN_FILE_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The OpenFile class represents a FUSE request for opening of a file.
 */
class OpenFile : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to be opened.
     * @param flags Open flags.
     */
    OpenFile(std::string uuid, const one::helpers::FlagsSet flags);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    one::helpers::FlagsSet m_flags;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_OPEN_FILE_H
