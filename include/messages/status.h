/**
 * @file status.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_STATUS_H
#define ONECLIENT_MESSAGES_STATUS_H

#include "messages/serverMessage.h"
#include "messages/clientMessage.h"

#include <memory>
#include <ostream>
#include <string>
#include <system_error>
#include <tuple>
#include <unordered_map>

namespace one {

namespace clproto {
class Status;
}

namespace messages {

/**
 * The Status class represents a message that is sent by the client or the
 * server
 * to inform about requested operation status.
 */
class Status : public ClientMessage, public ServerMessage {
public:
    /**
     * Constructor.
     * @param code Status code.
     */
    Status(std::error_code code);

    /**
     * Constructor.
     * @param code Status code.
     * @param description Status description.
     */
    Status(std::error_code code, std::string description);

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * HandshakeResponse counterpart.
     */
    Status(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Status code.
     */
    std::error_code code() const;

    /**
     * Translates the incoming status code.
     * @param status The status to translate.
     * @return Translated status.
     */
    static std::tuple<std::error_code, std::string> translate(
        const clproto::Status &status);

    /**
     * @return Status description.
     */
    const std::string &description() const;

    virtual std::string toString() const override;

    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::error_code m_code;
    std::string m_description;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_STATUS_H
