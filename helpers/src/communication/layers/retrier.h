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

#include <boost/thread/future.hpp>

#include <chrono>
#include <functional>
#include <memory>

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
    boost::future<void> send(std::string message, const int retries);
};

template <class LowerLayer>
boost::future<void> Retrier<LowerLayer>::send(
    std::string message, const int retries)
{
    auto future = LowerLayer::send(message, retries);
    auto retriedWrappedFuture = future.then(
        [ this, retries, message = std::move(message) ](auto f) mutable {
            try {
                f.get();
                return boost::make_ready_future();
            }
            catch (SendError) {
                if (retries == 0)
                    throw;

                return this->send(std::move(message), retries - 1);
            }
        });

    return retriedWrappedFuture.unwrap();
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_RETRIER_H
