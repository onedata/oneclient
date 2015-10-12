/**
 * @file getFileLocation.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H

#include "messages/clientMessage.h"

#include <boost/optional.hpp>

#include <sys/types.h>

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileLocation class represents a FUSE request for file location.
 */
class GetFileLocation : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the directory of which children are requested.
     */
    GetFileLocation(std::string uuid);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H
