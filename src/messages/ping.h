/**
 * @file ping.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PING_H
#define HELPERS_MESSAGES_PING_H

#include "clientMessage.h"

#include <boost/optional.hpp>

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The Ping class represents a message that is sent by the client to
 * establish session.
 */
class Ping : public ClientMessage {
public:
    Ping() = default;
    Ping(std::string data);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    boost::optional<std::string> m_data;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PING_H
