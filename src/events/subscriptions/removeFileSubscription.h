/**
 * @file removeFileSubscription.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOVE_FILE_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOVE_FILE_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/clientMessage.h"

#include <boost/optional.hpp>

#include <cstddef>
#include <sstream>

namespace one {
namespace client {
namespace events {

/**
 * @c RemoveFileSubscription is a client side subscription and represents
 * a request for remove_file events.
 */
class RemoveFileSubscription : public Subscription,
                               public messages::ClientMessage {
public:
    /**
     * Constructor.
     * @param fileUuid UUID of file for which remove change events are
     * requested.
     */
    RemoveFileSubscription(
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

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOVE_FILE_SUBSCRIPTION_H
