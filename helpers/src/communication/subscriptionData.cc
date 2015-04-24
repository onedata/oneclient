/**
 * @file subscriptionData.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionData.h"

namespace one {
namespace communication {

SubscriptionData::SubscriptionData(
    std::function<bool(const ServerMessage &, const bool)> predicate,
    std::function<void(const ServerMessage &)> callback)
    : predicate(std::move(predicate))
    , callback(std::move(callback))
{
}

} // namespace communication
} // namespace one
