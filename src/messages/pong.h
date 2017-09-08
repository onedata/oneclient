/**
 * @file pong.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PONG_H
#define HELPERS_MESSAGES_PONG_H

#include "serverMessage.h"

#include <boost/optional.hpp>

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The Pong class represents a message that is sent by the server to
 * confirm session establishment.
 */
class Pong : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * Pong counterpart.
     */
    Pong(std::unique_ptr<ProtocolServerMessage> serverMessage);

    const boost::optional<std::string> &data() const;

    virtual std::string toString() const override;

private:
    boost::optional<std::string> m_data;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PONG_H
