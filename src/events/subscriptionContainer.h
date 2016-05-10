/**
 * @file subscriptionContainer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTION_CONTAINER_H
#define ONECLIENT_EVENTS_SUBSCRIPTION_CONTAINER_H

#include "messages/serverMessage.h"
#include "subscriptions/readSubscription.h"
#include "subscriptions/writeSubscription.h"
#include "subscriptions/fileAccessedSubscription.h"

#include "messages.pb.h"

#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * The @c SubscriptionContainer class represents container for subscriptions.
 * It is used to store server subscriptions that arrived on handshake.
 */
class SubscriptionContainer : public messages::ServerMessage {
public:
    using ProtocolMessage = clproto::Subscription;

    /**
     * Translates and adds protocol subscription message.
     * @param message Protocol Buffers message representing server subscription.
     */
    void add(const ProtocolMessage &message);

    /**
     * Moves read event subscriptions from container by effectively emptying its
     * content.
     * @return subscriptions for read events.
     */
    std::vector<ReadSubscription> moveReadSubscriptions();

    /**
     * Moves write event subscriptions from container by effectively emptying
     * its content.
     * @return subscriptions for write events.
     */
    std::vector<WriteSubscription> moveWriteSubscriptions();

    /**
     * Moves file accessed event subscriptions from container
     * by effectively emptying its content.
     * @return subscriptions for file accessed events.
     */
    std::vector<FileAccessedSubscription> moveFileAccessedSubscription();

    virtual std::string toString() const override;

private:
    std::vector<ReadSubscription> m_readSubscriptions;
    std::vector<WriteSubscription> m_writeSubscriptions;
    std::vector<FileAccessedSubscription> m_fileAccessedSubscription;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTION_CONTAINER_H
