/**
 * @file binaryTranslator.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_BINARY_TRANSLATOR_H
#define HELPERS_COMMUNICATION_LAYERS_BINARY_TRANSLATOR_H

#include "logging.h"
#include "communication/declarations.h"

#include <functional>
#include <memory>
#include <system_error>

namespace one {
namespace communication {
namespace layers {

/**
 * @c BinaryTranslator is responsible for translating @c ClientMessage protobuf
 * messages to string representation, and string representation back to
 * @c ServerMessage protobuf messages.
 */
template <class LowerLayer> class BinaryTranslator : public LowerLayer {
public:
    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;
    using LowerLayer::send;
    virtual ~BinaryTranslator() = default;

    /**
     * A reference to @c *this typed as a @c BinaryTranslator.
     */
    BinaryTranslator<LowerLayer> &binaryTranslator = *this;

    /**
     * Wraps lower layer's @c setHandshake.
     * The handshake message is serialized into @c std::string and handshake
     * response is deserialized into a @c clproto::ServerMessage instance.
     * @see ConnectionPool::setHandshake()
     */
    auto setHandshake(std::function<ClientMessagePtr()> getHandshake,
        std::function<std::error_code(ServerMessagePtr)> onHandshakeResponse,
        std::function<void(std::error_code)> onHandshakeDone);

    /**
     * Wraps lower layer's @c setOnMessageCallback.
     * The incoming message is deserialized into a @c clproto::ServerMessage
     * instance.
     * @see ConnectionPool::setOnMessageCallback()
     */
    auto setOnMessageCallback(
        std::function<void(ServerMessagePtr)> onMessageCallback);

    /**
     * Serializes an instance of @c clproto::ClientMessage as @c std::string
     * and passes it down to the lower layer.
     * @param message The message to send.
     * @param retries The retries argument to pass to the lower layer.
     * @see ConnectionPool::send()
     */
    auto send(ClientMessagePtr message, Callback callback,
        const int retries = DEFAULT_RETRY_NUMBER);
};

template <class LowerLayer>
auto BinaryTranslator<LowerLayer>::setHandshake(
    std::function<ClientMessagePtr()> getHandshake,
    std::function<std::error_code(ServerMessagePtr)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
{
    return LowerLayer::setHandshake(
        [getHandshake = std::move(getHandshake)] {
            return getHandshake()->SerializeAsString();
        },

        [onHandshakeResponse = std::move(onHandshakeResponse)](
            std::string message) {
            /// @todo A potential place for optimization [static serverMsg]
            auto serverMsg = std::make_unique<clproto::ServerMessage>();

            if (!serverMsg->ParseFromString(message))
                return std::make_error_code(std::errc::protocol_error);

            return onHandshakeResponse(std::move(serverMsg));
        },

        std::move(onHandshakeDone));
}

template <class LowerLayer>
auto BinaryTranslator<LowerLayer>::setOnMessageCallback(
    std::function<void(ServerMessagePtr)> onMessageCallback)
{
    return LowerLayer::setOnMessageCallback([onMessageCallback =
                                                 std::move(onMessageCallback)](
        std::string message) {
        auto serverMsg = std::make_unique<clproto::ServerMessage>();
        if (serverMsg->ParseFromString(message)) {
            onMessageCallback(std::move(serverMsg));
        }
        else {
            DLOG(WARNING) << "Received an invalid message from the server: '"
                          << message.substr(0, 40)
                          << "' (message trimmed to 40 chars).";
        }
    });
}

template <class LowerLayer>
auto BinaryTranslator<LowerLayer>::send(
    ClientMessagePtr message, Callback callback, const int retries)
{
    /// @todo Possible optimization point here [static thread-local string]
    return LowerLayer::send(
        message->SerializeAsString(), std::move(callback), retries);
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_BINARY_TRANSLATOR_H
