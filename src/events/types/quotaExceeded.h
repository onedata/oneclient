/**
 * @file quotaExceeded.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H
#define ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H

#include "singleEvent.h"

#include <vector>

namespace one {
namespace clproto {
class QuotaExceededEvent;
} // namespace clproto
namespace client {
namespace events {

class QuotaExceeded : public SingleEvent {
    using ProtocolMessage = clproto::QuotaExceededEvent;

public:
    QuotaExceeded(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    const std::vector<std::string> &spaces() const;

    std::string toString() const override;

private:
    std::vector<std::string> m_spaces;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H
