/**
 * @file subscriptionHandle.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionHandle.h"
#include "events/streams/sharedStream.h"
#include "logging.h"

namespace one {
namespace client {
namespace events {

SubscriptionHandle::SubscriptionHandle(StreamKey streamKey, Streams &streams)
    : m_streamKey{streamKey}
    , m_streams{streams}
{
}

SubscriptionHandle::~SubscriptionHandle()
{
    StreamAcc acc;
    if (m_streams.find(acc, m_streamKey)) {
        if (acc->second->release()) {
            LOG_DBG(1) << "Removing stream '" << m_streamKey << "'";
            m_streams.erase(acc);
        }
    }
}

} // namespace events
} // namespace client
} // namespace one
