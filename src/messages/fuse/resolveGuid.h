/**
 * @file resolveGuid.h
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_RESOLVE_GUID_H
#define ONECLIENT_MESSAGES_FUSE_RESOLVE_GUID_H

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The ResolveGuid class represents a FUSE request for file attributes.
 */
class ResolveGuid : public ClientMessage {
public:
    /**
     * Constructor.
     * @param path Path of the file for which attributes are requested.
     */
    ResolveGuid(boost::filesystem::path path);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    boost::filesystem::path m_path;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_RESOLVE_GUID_H
