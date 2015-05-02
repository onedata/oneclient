/**
* @file eventSubscriptionCancellation.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_EVENT_SUBSCRIPTION_CANCELLATION_H
#define ONECLIENT_MESSAGES_EVENT_SUBSCRIPTION_CANCELLATION_H

#include "messages/serverMessage.h"

#include <chrono>
#include <memory>
#include <string>
#include <sys/types.h>

namespace one {
namespace client {
namespace events {

/**
* The EventSubscriptionCancellation class represents read event subscription
* request sent by the server.
*/
class EventSubscriptionCancellation : public one::messages::ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * EventSubscriptionCancellation counterpart.
     */
    EventSubscriptionCancellation(
        const messages::ProtocolServerMessage &serverMessage);

    /**
     * @return Id of subscription to be cancelled.
     */
    uint64_t id() const;

    virtual std::string toString() const override;

private:
    uint64_t m_id;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_EVENT_SUBSCRIPTION_CANCELLATION_H
