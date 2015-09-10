/**
 * @file close.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CLOSE_H
#define ONECLIENT_MESSAGES_FUSE_CLOSE_H

#include "messages/clientMessage.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c Close class represents a FUSE request for file closure.
 */
class Close : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to close.
     */
    Close(std::string uuid);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CLOSE_H
