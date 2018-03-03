/**
 * @file subscriptionData.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_SUBSCRIPTION_DATA_H
#define HELPERS_COMMUNICATION_SUBSCRIPTION_DATA_H

#include <functional>

namespace one {

namespace clproto {
class ServerMessage;
}

namespace communication {

/**
 * @c SubscriptionData is a pair holding predicate and callback for a single
 * subscription.
 * @see one::communication::layers::Inbox::subscribe()
 */
struct SubscriptionData {
    using ServerMessage = clproto::ServerMessage;

public:
    /**
     * Constructor.
     * @param predicate The predicate to determine if the @p callback should
     * be called. The boolean parameter states whether the message was
     * handled by the inbox.
     * @param callback The callback to be called if the @p predicate returns
     * true.
     */
    SubscriptionData(
        std::function<bool(const ServerMessage &, const bool)> predicate,
        std::function<void(const ServerMessage &)> callback);

    SubscriptionData() = default;

    /**
     * The stored predicate.
     */
    std::function<bool(const ServerMessage &, const bool)> predicate =
        [](auto &, auto) { return false; };

    /**
     * The stored callback.
     */
    std::function<void(const ServerMessage &)> callback;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_SUBSCRIPTION_DATA_H
