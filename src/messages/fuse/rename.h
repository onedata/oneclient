/**
 * @file rename.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_RENAME_H
#define ONECLIENT_MESSAGES_FUSE_RENAME_H

#include "fileRequest.h"

#include <boost/filesystem/path.hpp>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The Rename class represents a FUSE request for rename.
 */
class Rename : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to rename.
     * @param targetParentUuid Uuid of the new parent.
     * @param targetName New name of the file.
     */
    Rename(
        std::string uuid, std::string targetParentUuid, std::string targetName);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_targetParentUuid;
    std::string m_targetName;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_RENAME_H
