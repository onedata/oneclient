/**
 * @file singleEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "singleEvent.h"
#include "logging.h"

namespace one {
namespace client {
namespace events {

const AggregationKey &SingleEvent::aggregationKey() const
{
    return m_aggregationKey;
}

void SingleEvent::aggregate(EventPtr<> event)
{
    LOG_DBG(1) << "Aggregation requested for a single event: " << toString();
}

} // namespace events
} // namespace client
} // namespace one
