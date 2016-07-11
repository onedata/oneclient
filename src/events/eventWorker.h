/**
 * @file eventWorker.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_WORKER_H
#define ONECLIENT_EVENTS_EVENT_WORKER_H

#include "subscriptionRegistry.h"
#include "utils.hpp"

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <asio/io_service_strand.hpp>
#include <asio/post.hpp>

#include <functional>
#include <thread>

namespace one {
namespace client {
namespace events {

/**
 * @c EventWorker is a wrapper on @c asio::io_service. It is responsible for
 * processing events on dedicated worker thread.
 */
template <class LowerLayer> class EventWorker : public LowerLayer {
public:
    using EventT = typename LowerLayer::EventT;
    using EventPtr = typename LowerLayer::EventPtr;
    using Subscription = typename LowerLayer::Subscription;
    using RegistryPtr = std::shared_ptr<SubscriptionRegistry>;

    /**
     * Constructor.
     * Calls @c LowerLayer constructor and sets up IO service.
     */
    template <class... Args>
    EventWorker(Args &&... args)
        : LowerLayer{std::forward<Args>(args)...}
        , m_ioService{1}
        , m_idleWork{asio::make_work(m_ioService)}
        , m_worker{[=] {
            etls::utils::nameThread("EventWorker");
            m_ioService.run();
        }}
    {
        LowerLayer::setPeriodicTriggerHandler([this] {
            asio::post(m_ioService, [this] { LowerLayer::trigger(); });
        });
    }

    /**
     * Destructor.
     * Stops the IO service and joins worker thread.
     */
    virtual ~EventWorker();

    /**
     * Wraps lower layer's @c process.
     * Processes an event on the worker thread.
     */
    void emitEvent(EventT event);

    /**
     * Wraps lower layer's @c process.
     * Creates an event and processes it on the worker thread.
     */
    template <class... Args> void createAndEmitEvent(Args &&... args)
    {
        asio::post(m_ioService,
            [=] { LowerLayer::process(std::make_unique<EventT>(args...)); });
    }

    /**
     * Wraps lower layer's @c subscribe.
     * Processes subscription on the worker thread and registers unsubscribe
     * handler in the @c SubscriptionRegistry.
     * @param subscription A subscription to be added.
     */
    virtual void subscribe(Subscription &&subscription);

    int64_t subscribe(
        Subscription &&clientSubscription, Subscription &&serverSubscription);

    /**
     * Sets the @c SubscriptionRegistry shared between all event streams.
     * @param registry @c SubscriptionRegistry instance.
     */
    void setSubscriptionRegistry(RegistryPtr registry);

private:
    asio::io_service m_ioService;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::thread m_worker;
    RegistryPtr m_registry;
};

template <class LowerLayer> EventWorker<LowerLayer>::~EventWorker()
{
    m_ioService.stop();
    m_worker.join();
}

template <class LowerLayer>
void EventWorker<LowerLayer>::emitEvent(EventWorker::EventT event)
{
    asio::post(m_ioService, [ this, event = std::move(event) ]() mutable {
        LowerLayer::process(std::make_unique<EventT>(std::move(event)));
    });
}

template <class LowerLayer>
void EventWorker<LowerLayer>::subscribe(Subscription &&subscription)
{
    asio::post(m_ioService,
        [ this, subscription = std::move(subscription) ]() mutable {
            auto id = subscription.id();
            auto handler = LowerLayer::subscribe(
                std::make_unique<Subscription>(std::move(subscription)));
            m_registry->addUnsubscribeHandler(
                id, [ this, handler = std::move(handler) ]() mutable {
                    asio::post(m_ioService, std::move(handler));
                });
        });
}

template <class LowerLayer>
int64_t EventWorker<LowerLayer>::subscribe(
    Subscription &&clientSubscription, Subscription &&serverSubscription)
{
    auto id = m_registry->nextSubscriptionId();
    clientSubscription.id(id);
    subscribe(std::move(clientSubscription));
    serverSubscription.id(id);
    LowerLayer::send(std::move(serverSubscription));
    return id;
}

template <class LowerLayer>
void EventWorker<LowerLayer>::setSubscriptionRegistry(RegistryPtr registry)
{
    m_registry = std::move(registry);
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_WORKER_H
