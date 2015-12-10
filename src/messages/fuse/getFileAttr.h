/**
 * @file getFileAttr.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_H

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileAttr class represents a FUSE request for file attributes.
 */
class GetFileAttr : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which attributes are requested.
     */
    GetFileAttr(std::string uuid);

    /**
     * Constructor.
     * @param path Path of the file for which attributes are requested.
     */
    GetFileAttr(boost::filesystem::path path);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    boost::optional<std::string> m_uuid;
    boost::optional<boost::filesystem::path> m_path;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_H
