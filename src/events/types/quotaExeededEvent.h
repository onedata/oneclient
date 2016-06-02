/**
 * @file quotaExeededEvent.h
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_QUOTA_EXEEDED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_QUOTA_EXEEDED_EVENT_H

#include "event.h"

#include <string>
#include <vector>

namespace one {
namespace clproto {
    class QuotaExeededEvent;
}
namespace client {
namespace events {

class QuotaSubscription;

/**
 * @c QuotaExeededEvent class represents an event containing 
 * spaces in which quota has been exeeded. 
 */
class QuotaExeededEvent : public Event {
public:
    using EventPtr = std::unique_ptr<QuotaExeededEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::QuotaExeededEvent;
    using Subscription = QuotaSubscription;

    /**
     * Constructor.
     * @param message permisionChangedEvent protocol message.
     */
    QuotaExeededEvent(const ProtocolMessage &message);

    /**
     * @return Value that distinguish @c this read event from other read events,
     * i.e. read events with the same key can be aggregated.
     * @see @c QuotaExeededEvent::Key.
     */
    const Key &key() const;

    /**
     * @return ID of file associated with the read event.
     */
    const std::vector<std::string> &spaces() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event QuotaExeededEvent to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() override;

protected:
    std::vector<std::string> m_spaces;
    const std::string m_key = "";
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_QUOTA_EXEEDED_EVENT_H
