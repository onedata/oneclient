/**
 * @file makeLink.h
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_MAKE_LINK_H
#define ONECLIENT_MESSAGES_FUSE_MAKE_LINK_H

#include "fileRequest.h"

#include <fcntl.h>
#include <sys/types.h>

#include <helpers/storageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The MakeLink class represents a FUSE request for creation of a hard link.
 */
class MakeLink : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the source file.
     * @param parentUuid UUID of the parent directory of target link.
     * @param name Name of the new hard link.
     */
    MakeLink(const folly::fbstring &uuid, folly::fbstring parentUuid,
        folly::fbstring name);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_parentUuid;
    folly::fbstring m_name;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_MAKE_LINK_H
