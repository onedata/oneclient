/**
 * @file translator.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H
#define HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H

#include "communication/declarations.h"
#include "messages/clientMessage.h"
#include "messages/serverMessage.h"
#include "messages/handshakeRequest.h"
#include "messages/handshakeResponse.h"

#include <atomic>
#include <cassert>
#include <chrono>
#include <functional>
#include <future>
#include <system_error>

namespace one {
namespace communication {

constexpr std::chrono::seconds DEFAULT_TIMEOUT{60};

namespace layers {

/**
 * @c Translator is responsible for translating between domain objects and
 * @c clproto objects.
 */
template <class LowerLayer> class Translator : public LowerLayer {
public:
    using LowerLayer::LowerLayer;
    using LowerLayer::send;

    template <typename SrvMsg>
    using CommunicateCallback =
        std::function<void(const std::error_code &ec, std::unique_ptr<SrvMsg>)>;

    virtual ~Translator() = default;

    /**
     * A reference to @c *this typed as a @c Translator.
     */
    Translator<LowerLayer> &translator = *this;

    /**
     * Serializes an instance of @c message::client::ClientMessage as
     * @c clproto::ClientMessage and passes it down to the lower layer.
     * @param message The message to send.
     * @param retires The retries argument to pass to the lower layer.
     * @see ConnectionPool::send()
     */
    auto send(messages::ClientMessage &&msg,
        const int retries = DEFAULT_RETRY_NUMBER);

    /**
     * Wraps lower layer's @c setHandshake.
     * The handshake message is serialized into @c one::clproto::ClientMessage
     * and handshake response is deserialized into a
     * @c one::clproto::ServerMessage instance.
     * @see ConnectionPool::setHandshake()
     * @note This method is only instantiable if the lower layer has a
     * @c setHandshake method.
     * @return A future containing the status of the first handshake.
     */
    template <typename = void>
    auto setHandshake(
        std::function<one::messages::HandshakeRequest()> getHandshake,
        std::function<std::error_code(one::messages::HandshakeResponse)>
            onHandshakeResponse)
    {
        auto hasBeenSet = std::make_shared<std::atomic<int>>(0);
        auto promise = std::make_shared<std::promise<void>>();

        LowerLayer::setHandshake(
            [getHandshake = std::move(getHandshake)] {
                return messages::serialize(getHandshake());
            },
            [onHandshakeResponse = std::move(onHandshakeResponse)](
                ServerMessagePtr msg) {
                return onHandshakeResponse({std::move(msg)});
            },
            [=](const std::error_code &ec) mutable {
                // The promise has been already set.
                if (*hasBeenSet)
                    return;

                // In case of concurrent access, only one thread will get 1 from
                // incrementation.
                if (++(*hasBeenSet) != 1)
                    return;

                if (ec) {
                    promise->set_exception(
                        std::make_exception_ptr(std::system_error{ec}));
                }
                else {
                    promise->set_value();
                }
            });

        return promise->get_future();
    }

    /**
     * Wraps lower layer's @c reply.
     * The outgoing message is serialized as in @c send().
     * @see Replier::reply()
     * @note This method is only instantiable if the lower layer has a @c reply
     * method.
     */
    template <typename = void>
    auto reply(const clproto::ServerMessage &replyTo,
        messages::ClientMessage &&msg, const int retry = DEFAULT_RETRY_NUMBER)
    {
        auto promise = std::make_shared<std::promise<void>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            const std::error_code &ec) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else
                promise->set_value();
        };

        LowerLayer::reply(replyTo, messages::serialize(std::move(msg)),
            std::move(callback), retry);

        return future;
    }

    /**
     * Wraps lower layer's @c communicate.
     * The ougoing message is serialized as in @c send().
     * @see Inbox::communicate()
     * @note This method is only instantiable if the lower layer has a
     * @c communicate method.
     * @return A future representing peer's answer.
     */
    template <class SvrMsg>
    auto communicate(
        messages::ClientMessage &&msg, const int retries = DEFAULT_RETRY_NUMBER)
    {
        auto promise = std::make_shared<std::promise<SvrMsg>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            const std::error_code &ec, ServerMessagePtr protoMessage) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else {
                try {
                    promise->set_value(SvrMsg{std::move(protoMessage)});
                }
                catch (const std::exception &e) {
                    promise->set_exception(std::current_exception());
                }
            }
        };

        LowerLayer::communicate(
            messages::serialize(std::move(msg)), std::move(callback), retries);

        return future;
    }

    /**
     * Wraps lower layer's @c communicate.
     * The outgoing message is serialized as in @c send().
     * @see Inbox::communicate()
     * @note This method is only instantiable if the lower layer has a
     * @c communicate method.
     */
    template <class SvrMsg>
    auto communicate(messages::ClientMessage &&msg,
        CommunicateCallback<SvrMsg> callback,
        const int retries = DEFAULT_RETRY_NUMBER)
    {
        auto wrappedCallback = [callback = std::move(callback)](
            const std::error_code &ec, ServerMessagePtr protoMessage)
        {
            if (ec) {
                callback(ec, {});
            }
            else {
                try {
                    callback(
                        ec, std::make_unique<SvrMsg>(std::move(protoMessage)));
                }
                catch (const std::system_error &err) {
                    callback(err.code(), {});
                }
                catch (const std::exception &e) {
                    callback(std::make_error_code(std::errc::io_error), {});
                }
            }
        };

        return LowerLayer::communicate(messages::serialize(std::move(msg)),
            std::move(wrappedCallback), retries);
    }
};

template <class LowerLayer>
auto Translator<LowerLayer>::send(
    messages::ClientMessage &&msg, const int retries)
{
    auto promise = std::make_shared<std::promise<void>>();
    auto future = promise->get_future();

    auto callback = [promise = std::move(promise)](
        const std::error_code &ec) mutable
    {
        if (ec)
            promise->set_exception(
                std::make_exception_ptr(std::system_error{ec}));
        else
            promise->set_value();
    };

    LowerLayer::send(
        messages::serialize(std::move(msg)), std::move(callback), retries);

    return future;
}

} // namespace layers

/**
 * Waits for a future value, throwing a system_error timed_out exception if the
 * timeout has been exceeded.
 * @param msg The future to wait for.
 * @param timeout The timeout to wait for.
 * @returns The value of @c msg.get().
 */
template <class SvrMsg, typename Rep, typename Period>
SvrMsg wait(
    std::future<SvrMsg> &msg, std::chrono::duration<Rep, Period> timeout)
{
    const auto status = msg.wait_for(timeout);
    assert(status != std::future_status::deferred);

    if (status == std::future_status::timeout)
        throw std::system_error{std::make_error_code(std::errc::timed_out)};

    return msg.get();
}

/**
 * A convenience overload for @c wait.
 * Calls @c wait with @c DEFAULT_TIMEOUT.
 */
template <class SvrMsg> SvrMsg wait(std::future<SvrMsg> &msg)
{
    return wait(msg, DEFAULT_TIMEOUT);
}

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H
