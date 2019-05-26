/**
 * @file macaroon.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_MACAROON_H
#define ONECLIENT_MESSAGES_MACAROON_H

#include "messages/clientMessage.h"

namespace one {
namespace messages {

/**
 * The @c Macaroon class represents a new macaroon for connection.
 */
class Macaroon : public ClientMessage {
public:
    /**
     * Constructor.
     * @param macaroon The macaroon to send.
     */
    Macaroon(std::string macaroon);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_macaroon;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_MACAROON_H
