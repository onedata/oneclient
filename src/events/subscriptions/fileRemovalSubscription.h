/**
 * @file fileRemovalSubscription.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_REMOVAL_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_REMOVAL_SUBSCRIPTION_H

#include "messages/clientMessage.h"
#include "subscription.h"

#include <cstddef>

namespace one {
namespace client {
namespace events {

/**
 * @c FileRemovalSubscription is a client side subscription and represents
 * a request for @c FileRemovalEvent.
 */
class FileRemovalSubscription : public Subscription,
                                public messages::ClientMessage {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of file for which file removal events are requested.
     */
    FileRemovalSubscription(
        std::string fileUuid, std::size_t counterThreshold = 1);

    std::string toString() const override;

private:
    std::unique_ptr<one::messages::ProtocolClientMessage>
    serializeAndDestroy() override;

    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_REMOVAL_SUBSCRIPTION_H
