/**
 * @file inbox.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_INBOX_H
#define HELPERS_COMMUNICATION_LAYERS_INBOX_H

#include "communication/declarations.h"
#include "communication/subscriptionData.h"

#include <boost/thread/future.hpp>
#include <tbb/concurrent_hash_map.h>
#include <tbb/concurrent_queue.h>
#include <tbb/concurrent_vector.h>

#include <atomic>
#include <functional>
#include <memory>
#include <string>

namespace one {
namespace communication {
namespace layers {

/**
 * @c Inbox is responsible for handling incoming messages. It stores a
 * collection of unfulfilled promises and matches them to incoming messages
 * by a message id. Other objects can subscribe on messages, e.g. to create
 * a 'push message' communication channel.
 */
template <class LowerLayer> class Inbox : public LowerLayer {
public:
    using LowerLayer::LowerLayer;
    virtual ~Inbox() = default;

    /**
     * A reference to @c *this typed as an @c Inbox.
     */
    Inbox<LowerLayer> &inbox = *this;

    /**
     * Sends a message to the server and sets up to receive a reply.
     * @param message The message to be sent.
     * @param retries Number of retries in case of sending error.
     * @return A future which should be fulfiled with server's reply.
     */
    boost::future<ServerMessagePtr> communicate(
        ClientMessagePtr message, const int retries = DEFAULT_RETRY_NUMBER);

    /**
     * Subscribes a given callback to messages received from the server.
     * Message can be received through any connection.
     * The @c SubscriptionData::predicate is first called to determine if
     * the @c SubscriptionData::callback should be called.
     * @param data A structure holding the predicate and callback functions.
     * @return A function to cancel the subscription.
     * @note Subscription callbacks should be lightweight, as they are handled
     * in communicator's threads.
     */
    std::function<void()> subscribe(SubscriptionData data);

    /**
     * Wraps lower layer's @c connect.
     * Sets a custom @c setOnMessageCallback on all lower layers. This layer
     * does not provide a @c setOnMessageCallback method.
     * @see ConnectionPool::connect()
     */
    auto connect();

    void setOnMessageCallback(std::function<void(ServerMessagePtr)>) = delete;

private:
    tbb::concurrent_hash_map<std::string,
        std::shared_ptr<boost::promise<ServerMessagePtr>>> m_promises;

    /// The counter will loop after sending ~65000 messages, providing us with
    /// a natural size bound for m_promises.
    std::atomic<std::uint16_t> m_nextMsgId{0};

    tbb::concurrent_vector<SubscriptionData> m_subscriptions;
    tbb::concurrent_queue<typename decltype(m_subscriptions)::iterator>
        m_unusedSubscriptions;
};

template <class LowerLayer>
boost::future<ServerMessagePtr> Inbox<LowerLayer>::communicate(
    ClientMessagePtr message, const int retries)
{
    const auto messageId = std::to_string(m_nextMsgId++);
    message->set_message_id(messageId);

    auto promise = std::make_shared<boost::promise<ServerMessagePtr>>();
    typename decltype(m_promises)::accessor acc;
    m_promises.insert(acc, messageId);
    acc->second = promise;
    acc.release();

    auto sendFuture = LowerLayer::send(std::move(message), retries);
    auto future = sendFuture.then(*LowerLayer::m_ioServiceExecutor,
        [this, promise, messageId](auto f) mutable {
            if (f.has_exception()) {
                this->m_promises.erase(messageId);
                f.get();
            }

            return promise->get_future();
        });

    return future.unwrap();
}

template <class LowerLayer>
std::function<void()> Inbox<LowerLayer>::subscribe(SubscriptionData data)
{
    typename decltype(m_subscriptions)::iterator it;
    if (m_unusedSubscriptions.try_pop(it))
        *it = std::move(data);
    else
        m_subscriptions.emplace_back(std::move(data));

    return [this, it] {
        *it = SubscriptionData{};
        m_unusedSubscriptions.push(it);
    };
}

template <class LowerLayer> auto Inbox<LowerLayer>::connect()
{
    LowerLayer::setOnMessageCallback([this](ServerMessagePtr message) {
        typename decltype(m_promises)::accessor acc;
        const bool handled = m_promises.find(acc, message->message_id());

        for (const auto &sub : m_subscriptions)
            if (sub.predicate(*message, handled))
                sub.callback(*message);

        if (handled) {
            auto promise = std::move(acc->second);
            m_promises.erase(acc);
            promise->set_value(std::move(message));
        }
    });

    return LowerLayer::connect();
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_INBOX_H
