/**
 * @file helperParamsChanged.cc
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helperParamsChanged.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

HelperParamsChanged::HelperParamsChanged(const ProtocolMessage &msg)
    : m_storageId{msg.storage_id()}
    , m_aggregationKey{m_storageId}
{
}

StreamKey HelperParamsChanged::streamKey() const
{
    return StreamKey::HELPER_PARAMS_CHANGED;
}

const AggregationKey &HelperParamsChanged::aggregationKey() const
{
    return m_aggregationKey;
}

const std::string &HelperParamsChanged::storageId() const
{
    return m_storageId;
}

std::string HelperParamsChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'HelperParamsChanged', storageId: " << m_storageId;
    return stream.str();
}

void HelperParamsChanged::aggregate(EventPtr<HelperParamsChanged> event)
{
    m_storageId = event->m_storageId;
}

} // namespace events
} // namespace client
} // namespace one
