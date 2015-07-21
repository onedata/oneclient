/**
 * @file changeMode.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef MESSAGES_FUSE_CHANGE_MODE_H
#define MESSAGES_FUSE_CHANGE_MODE_H

#include "messages/clientMessage.h"

#include <sys/types.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The ChangeMode class represents a FUSE request for chmod.
 */
class ChangeMode : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to change the mode of.
     * @param mode File mode to set on the file.
     */
    ChangeMode(std::string uuid, mode_t mode);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
    mode_t m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // MESSAGES_FUSE_CHANGE_MODE_H
