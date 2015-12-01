/**
 * @file truncate.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_TRUNCATE_H
#define ONECLIENT_MESSAGES_FUSE_TRUNCATE_H

#include "messages/clientMessage.h"

#include <sys/types.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c Truncate class represents a FUSE request for file truncation.
 */
class Truncate : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file to close.
     * @param size The size to truncate the file to.
     */
    Truncate(std::string uuid, const off_t size);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
    off_t m_size;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_TRUNCATE_H
