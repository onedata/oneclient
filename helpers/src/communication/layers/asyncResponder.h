/**
 * @file asyncResponder.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_ASYNC_RESPONDER_H
#define HELPERS_COMMUNICATION_LAYERS_ASYNC_RESPONDER_H

#include "communication/declarations.h"
#include "communication/etls/utils.h"

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <asio/post.hpp>
#include <asio/wrap.hpp>

#include <functional>
#include <iostream>
#include <memory>

namespace one {
namespace communication {
namespace layers {

/**
 * @c AsyncResponder is responsible for offloading response callbacks to
 * separate threads.
 */
template <class LowerLayer> class AsyncResponder : public LowerLayer {
public:
    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;

    virtual ~AsyncResponder();

    /**
     * A reference to @c *this typed as a @c AsyncResponder.
     */
    AsyncResponder<LowerLayer> &asyncResponder = *this;

    /*
     * Wraps lower layer's @c connect.
     * Starts the thread that will run onMessage callbacks.
     * @see ConnectionPool::connect()
     */
    auto connect();

    /**
     * Runs higher layer's message callback in a separate thread.
     * @see ConnectionPool::setOnMessageCallback()
     */
    auto setOnMessageCallback(
        std::function<void(ServerMessagePtr)> onMessageCallback);

private:
    asio::io_service m_ioService;
    std::unique_ptr<asio::executor_work<asio::io_service::executor_type>>
        m_work;
    std::thread m_thread;
};

template <class LowerLayer> AsyncResponder<LowerLayer>::~AsyncResponder()
{
    m_ioService.stop();
    if (m_thread.joinable())
        m_thread.join();
}

template <class LowerLayer> auto AsyncResponder<LowerLayer>::connect()
{
    m_work =
        std::make_unique<asio::executor_work<asio::io_service::executor_type>>(
            asio::make_work(m_ioService));

    m_thread = std::thread{[this] {
        etls::utils::nameThread("AsyncResponder");
        m_ioService.run();
    }};

    return LowerLayer::connect();
}

template <class LowerLayer>
auto AsyncResponder<LowerLayer>::setOnMessageCallback(
    std::function<void(ServerMessagePtr)> onMessageCallback)
{
    return LowerLayer::setOnMessageCallback(
        [ this, onMessageCallback = std::move(onMessageCallback) ](
            ServerMessagePtr serverMsg) mutable {

            asio::post(
                m_ioService, [&, serverMsg = std::move(serverMsg) ]() mutable {
                    onMessageCallback(std::move(serverMsg));
                });
        });
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_ASYNC_RESPONDER_H
