/**
 * @file rename.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_RENAME_H
#define ONECLIENT_MESSAGES_FUSE_RENAME_H

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The Rename class represents a FUSE request for rename.
 */
class Rename : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to rename.
     * @param targetPath The new file path.
     */
    Rename(std::string uuid, boost::filesystem::path targetPath);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
    boost::filesystem::path m_targetPath;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_RENAME_H
