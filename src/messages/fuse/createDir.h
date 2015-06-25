/**
 * @file createDir.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_CREATE_DIR_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_CREATE_DIR_H

#include "messages/clientMessage.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreateDir class represents a FUSE request for dir creation.
 */
class CreateDir : public ClientMessage {
public:
    /**
     * Constructor.
     * @param parentUUID
     * @param name
     * @param mode
     */
    CreateDir(std::string parentUUID, std::string name, std::uint32_t mode);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_parentUUID;
    std::string m_name;
    std::uint32_t m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_CREATE_DIR_H
