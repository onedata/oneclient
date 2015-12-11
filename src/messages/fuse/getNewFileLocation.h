/**
 * @file getNewFileLocation.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_NEW_FILE_LOCATION_H
#define ONECLIENT_MESSAGES_FUSE_GET_NEW_FILE_LOCATION_H

#include "messages/clientMessage.h"

#include <sys/types.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetNewFileLocation class represents a FUSE request for creation of a new
 * file.
 */
class GetNewFileLocation : public ClientMessage {
public:
    /**
     * Constructor.
     * @param name Name of the file to create.
     * @param parentUuid UUID of the directory in which to create the file.
     * @param mode Mode of the newly created file.
     */
    GetNewFileLocation(std::string name, std::string parentUuid, mode_t mode);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_name;
    std::string m_parentUuid;
    mode_t m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_NEW_FILE_LOCATION_H
