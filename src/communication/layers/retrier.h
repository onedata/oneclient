/**
 * @file retrier.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_RETRIER_H
#define HELPERS_COMMUNICATION_LAYERS_RETRIER_H

#include "communication/exception.h"

#include <chrono>
#include <functional>
#include <memory>
#include <string>
#include <system_error>

namespace one {
namespace communication {
namespace layers {

constexpr std::chrono::seconds DEFAULT_SEND_TIMEOUT{5};

/**
 * Retrier is responsible for retrying message send operation handled by
 * a lower layer.
 */
template <class LowerLayer> class Retrier : public LowerLayer {
public:
    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;
    virtual ~Retrier() = default;

    /**
     * A reference to @c *this typed as a @c Retrier.
     */
    Retrier<LowerLayer> &retrier = *this;

    /**
     * Sends a message with retries.
     * This method returns once a message is received by the remote endpoint,
     * or an error occurs and the number of retries has been exhausted.
     * If the message cannot be sent, last exception is stored in the future.
     * @param message The message to send.
     * @param retries The number of retries.
     * @return same as lower layer's @c send().
     * @see ConnectionPool::send()
     */
    void send(std::string message, Callback callback, const int retries);
};

template <class LowerLayer>
void Retrier<LowerLayer>::send(
    std::string message, Callback callback, const int retries)
{
    auto wrappedCallback =
        [ =, callback = std::move(callback) ](const std::error_code &ec) mutable
    {
        if (ec && retries > 0)
            send(std::move(message), std::move(callback), retries - 1);
        else
            callback(ec);
    };

    LowerLayer::send(std::move(message), std::move(wrappedCallback), retries);
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_RETRIER_H
