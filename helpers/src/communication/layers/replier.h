/**
 * @file replier.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_REPLIER_H
#define HELPERS_COMMUNICATION_LAYERS_REPLIER_H

#include "communication/declarations.h"

#include <system_error>

namespace one {
namespace communication {
namespace layers {

/**
 * @c Replier is responsible for providing a @c reply method built on lower
 * layers' @c send().
 */
template <class LowerLayer> class Replier : public LowerLayer {
public:
    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;
    virtual ~Replier() = default;

    /**
     * A reference to @c *this typed as a @c Replier.
     */
    Replier<LowerLayer> &replier = *this;

    /**
     * Sends a message as a reply to given @c ServerMessage instance.
     * @param replyTo The message to reply to.
     * @param msg The reply message.
     * @param retry The number of times to retry on error.
     * @return same as lower layer's @c send().
     * @see ConnectionPool::send()
     */
    auto reply(const clproto::ServerMessage &replyTo, ClientMessagePtr msg,
        Callback callback, const int retry = DEFAULT_RETRY_NUMBER);
};

template <class LowerLayer>
auto Replier<LowerLayer>::reply(const clproto::ServerMessage &replyTo,
    ClientMessagePtr msg, Callback callback, const int retry)
{
    msg->set_message_id(replyTo.message_id());
    return LowerLayer::send(std::move(msg), std::move(callback), retry);
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_REPLIER_H
