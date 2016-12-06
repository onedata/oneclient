/**
 * @file singleEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

class SingleEvent : public Event {
public:
    const std::string &aggregationKey() const override;

    void aggregate(EventPtr<> event);

private:
    std::string m_aggregationKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H
