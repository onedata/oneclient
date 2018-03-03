/**
 * @file status.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_STATUS_H
#define HELPERS_MESSAGES_STATUS_H

#include "messages/clientMessage.h"
#include "messages/serverMessage.h"

#include <boost/optional.hpp>

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
 * server to inform about requested operation status.
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
     * Status counterpart.
     */
    Status(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * Constructor.
     * @param status Protocol Buffers message representing the status.
     */
    Status(clproto::Status &status);

    /**
     * @return Status code.
     */
    std::error_code code() const;

    /**
     * Throws an appropriate instance of @c std::system_error if the status
     * represents an error.
     */
    void throwOnError() const;

    /**
     * @return Status description.
     */
    const boost::optional<std::string> &description() const;

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::error_code m_code;
    boost::optional<std::string> m_description;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_STATUS_H
