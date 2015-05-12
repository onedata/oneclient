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

#include <boost/optional.hpp>

#include <memory>
#include <string>
#include <ostream>
#include <unordered_map>

namespace one {
namespace messages {

/**
* The Status class represents a message that is sent by the client or the server
* to inform about requested operation status.
*/
class Status : public ClientMessage, public ServerMessage {
public:
    enum class Code {
        ok,        // ok
        enoent,    // no such file or directory
        eacces,    // permission denied
        eexist,    // file exists
        eio,       // input/output error
        enotsup,   // operation not supported
        enotempty, // directory not empty
        eremoteio, // remote input/output error
        eperm,     // operation not permitted
        einval,    // invalid argument
        edquot,    // disc quota exceeded
        enoattr,   // attribute not found
        ecomm      // communication error on send
    };

    /**
     * Constructor.
     * @param code Status code.
     * */
    Status(Code code);

    /**
     * Constructor.
     * @param code Status code.
     * @param description Status description.
     */
    Status(Code code, std::string description);

    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * HandshakeResponse counterpart.
    */
    Status(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Status code.
     */
    Code code() const;

    /**
     * @return Status description.
     */
    const boost::optional<std::string> &description() const;

    virtual std::string toString() const override;

    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    Code m_code;
    boost::optional<std::string> m_description;
};

std::ostream &operator<<(std::ostream &stream, Status::Code code);

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_STATUS_H
