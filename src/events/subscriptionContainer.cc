/**
 * @file subscriptionContainer.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionContainer.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

void SubscriptionContainer::add(const ProtocolMessage &message)
{
    const auto id = message.id();
    if (message.has_read_subscription())
        m_readSubscriptions.emplace_back(id, message.read_subscription());
    else if (message.has_write_subscription())
        m_writeSubscriptions.emplace_back(id, message.write_subscription());
    else if (message.has_file_accessed_subscription())
        m_fileAccessedSubscription.emplace_back(
            id, message.file_accessed_subscription());
}

std::vector<ReadSubscription> SubscriptionContainer::moveReadSubscriptions()
{
    return std::move(m_readSubscriptions);
}

std::vector<WriteSubscription> SubscriptionContainer::moveWriteSubscriptions()
{
    return std::move(m_writeSubscriptions);
}

std::vector<FileAccessedSubscription>
SubscriptionContainer::moveFileAccessedSubscription()
{
    return std::move(m_fileAccessedSubscription);
}

std::string SubscriptionContainer::toString() const
{
    std::stringstream stream;

    stream << "type: 'SubscriptionContainer', read subscriptions: [";
    for (const auto &subscription : m_readSubscriptions)
        stream << subscription.toString() << ", ";
    stream << "], write subscriptions: [";
    for (const auto &subscription : m_writeSubscriptions)
        stream << subscription.toString() << ", ";
    stream << "], file accessed subscriptions: [";
    for (const auto &subscription : m_fileAccessedSubscription)
        stream << subscription.toString() << ", ";
    stream << "]";

    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
