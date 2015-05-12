/**
 * @file translator.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H
#define HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H

#include "communication/future.h"
#include "communication/declarations.h"
#include "messages/clientMessage.h"
#include "messages/serverMessage.h"
#include "messages/handshakeRequest.h"
#include "messages/handshakeResponse.h"

#include <boost/thread/future.hpp>

#include <functional>

namespace one {
namespace communication {
namespace layers {

/**
 * @c Translator is responsible for translating between domain objects and
 * @c clproto objects.
 */
template <class LowerLayer> class Translator : public LowerLayer {
public:
    using LowerLayer::LowerLayer;
    using LowerLayer::send;
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
    auto send(const messages::ClientMessage &msg,
        const int retries = DEFAULT_RETRY_NUMBER);

    /**
     * Wraps lower layer's @c setHandshake.
     * The handshake message is serialized into @c one::clproto::ClientMessage
     * and handshake response is deserialized into a
     * @c one::clproto::ServerMessage instance.
     * @see ConnectionPool::setHandshake()
     * @note This method is only instantiable if the lower layer has a
     * @c setHandshake method.
     */
    template <typename = void>
    auto setHandshake(
        std::function<one::messages::HandshakeRequest()> getHandshake,
        std::function<bool(one::messages::HandshakeResponse)>
            onHandshakeResponse)
    {
        return LowerLayer::setHandshake(
            [getHandshake = std::move(getHandshake)] {
                return getHandshake().serialize();
            },
            [onHandshakeResponse = std::move(onHandshakeResponse)](
                ServerMessagePtr msg) {
                return onHandshakeResponse({std::move(msg)});
            });
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
        const messages::ClientMessage &msg,
        const int retry = DEFAULT_RETRY_NUMBER)
    {
        auto protoMsg = msg.serialize();
        return wrapFuture(
            LowerLayer::reply(replyTo, std::move(protoMsg), retry));
    }

    /**
     * Wraps lower layer's @c communicate.
     * The ougoing message is serialized as in @c send().
     * @see Inbox::communicate()
     * @note This method is only instantiable if the lower layer has a
     * @c communicate method.
     */
    template <class SvrMsg>
    auto communicate(const messages::ClientMessage &msg,
        const int retries = DEFAULT_RETRY_NUMBER)
    {
        auto protoMsg = msg.serialize();
        auto future = LowerLayer::communicate(std::move(protoMsg), retries);
        return wrapFuture(future.then(LowerLayer::m_ioServiceExecutor,
            [](auto f) { return SvrMsg{f.get()}; }));
    }
};

template <class LowerLayer>
auto Translator<LowerLayer>::send(
    const messages::ClientMessage &msg, const int retries)
{
    auto protoMsg = msg.serialize();
    return wrapFuture(LowerLayer::send(std::move(protoMsg), retries));
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H
