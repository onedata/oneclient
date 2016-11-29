/**
 * @file subscriptionHandle.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionHandle.h"

namespace one {
namespace client {
namespace events {

SubscriptionHandle::SubscriptionHandle(
    std::int64_t streamId, std::string routingKey, Router &router)
    : m_streamId{streamId}
    , m_routingKey{std::move(routingKey)}
    , m_router{router}
{
    RouterAcc acc;
    m_router.insert(acc, m_routingKey);
    acc->second.insert(m_streamId);
}

SubscriptionHandle::~SubscriptionHandle()
{
    RouterAcc acc;
    if (m_router.find(acc, m_routingKey)) {
        acc->second.erase(m_streamId);
        if (acc->second.empty()) {
            m_router.erase(acc);
        }
    }
}

} // namespace events
} // namespace client
} // namespace one
