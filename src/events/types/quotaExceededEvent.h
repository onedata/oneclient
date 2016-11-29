/**
 * @file quotaExceededEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_EVENT_H

#include "event.h"

#include <vector>

namespace one {
namespace clproto {
class QuotaExceededEvent;
} // namespace clproto
namespace client {
namespace events {

class QuotaExceededEvent : public Event {
    using ProtocolMessage = clproto::QuotaExceededEvent;

public:
    QuotaExceededEvent(const ProtocolMessage &msg);

    const std::string &routingKey() const override;

    const std::string &aggregationKey() const override;

    const std::vector<std::string> &spaces() const;

    std::string toString() const override;

    void aggregate(ConstEventPtr event) override;

    EventPtr clone() const override;

private:
    std::vector<std::string> m_spaces;
    std::string m_routingKey{"QuotaExceededEventStream"};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_EVENT_H
