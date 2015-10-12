/**
 * @file subscriptionRegistry.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/subscriptionRegistry.h"
#include "events/subscriptions/eventSubscriptionCancellation.h"

#include "context.h"
#include "scheduler.h"

#include <memory>

namespace one {
namespace client {
namespace events {

SubscriptionRegistry::SubscriptionRegistry(std::shared_ptr<Context> ctx)
    : m_ctx{std::move(ctx)}
    , m_strand{m_ctx.lock()->scheduler()->getIoService()}
{
}

std::future<bool> SubscriptionRegistry::add(
    std::pair<uint64_t, std::function<void()>> evtSub)
{
    auto promise = std::make_shared<std::promise<bool>>();
    auto future = promise->get_future();
    m_ctx.lock()->scheduler()->post(m_strand,
        [ this, promise = std::move(promise), evtSub = std::move(evtSub) ] {
            const auto evtSubCan = evtSub.second;
            const auto &inserted = m_subById.emplace(std::move(evtSub));
            if (!inserted.second)
                evtSubCan();
            promise->set_value(inserted.second);
        });
    return std::move(future);
}

std::future<bool> SubscriptionRegistry::remove(
    EventSubscriptionCancellation evtSubCan)
{
    auto promise = std::make_shared<std::promise<bool>>();
    auto future = promise->get_future();
    m_ctx.lock()->scheduler()->post(m_strand,
        [
          this,
          promise = std::move(promise),
          evtSubCan = std::move(evtSubCan)
        ] {
            const auto &found = m_subById.find(evtSubCan.id());
            if (found != m_subById.end()) {
                found->second();
                m_subById.erase(found);
                promise->set_value(true);
            }
            else
                promise->set_value(false);

        });
    return std::move(future);
}

} // namespace events
} // namespace client
} // namespace one
