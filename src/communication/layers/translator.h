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
#include "messages/client/clientMessage.h"
#include "messages/server/serverMessage.h"

#include <future>
#include <type_traits>

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
    auto send(const messages::client::ClientMessage &msg,
        const int retries = DEFAULT_RETRY_NUMBER);

    /**
     * Wraps lower layer's @c reply.
     * The outgoing message is serialized as in @c send().
     * @see Replier::reply()
     * @note This method is only instantiable if the lower layer has a @c reply
     * method.
     */
    template <typename = void>
    auto reply(clproto::ServerMessage &replyTo,
        const messages::client::ClientMessage &msg, const int retry)
    {
        auto protoMsg = msg.createSerializer()->serialize(msg);
        return LowerLayer::reply(replyTo, std::move(protoMsg), retry);
    }

    /**
     * Wraps lower layer's @c communicate.
     * The ougoing message is serialized as in @c send().
     * @see Inbox::communicate()
     * @note This method is only instantiable if the lower layre has a
     * @c communicate method.
     */
    template <class SvrMsg>
    std::future<SvrMsg> communicate(
        const messages::client::ClientMessage &msg, const int retries)
    {
        auto protoMsg = msg.createSerializer()->serialize(msg);
        auto future = LowerLayer::communicate(std::move(protoMsg), retries);
        return std::async(
            std::launch::deferred, [future = std::move(future)]() mutable {
                return SvrMsg{future.get()};
            });
    }
};

template <class LowerLayer>
auto Translator<LowerLayer>::send(
    const messages::client::ClientMessage &msg, const int retries)
{
    auto protoMsg = msg.createSerializer()->serialize(msg);
    return LowerLayer::send(std::move(protoMsg), retries);
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_TRANSLATOR_H
