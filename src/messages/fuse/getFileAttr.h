/**
 * @file getFileAttr.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_GET_FILE_ATTR_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_GET_FILE_ATTR_H

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
    GetFileAttr(std::string uuid);
    GetFileAttr(boost::filesystem::path path);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    boost::optional<std::string> m_uuid;
    boost::optional<boost::filesystem::path> m_path;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_DELETE_FILE_H
