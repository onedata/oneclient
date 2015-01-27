/**
* @file eventEmissionConfirmation.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_EMISSION_CONFIRMATION_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_EMISSION_CONFIRMATION_H

#include "events.pb.h"

#include <memory>

namespace one {
namespace client {
namespace events {

class EventBuffer;

static const std::string EVENT_EMISSION_CONFIRMATION_MESSAGE =
    one::clproto::events::EventEmissionConfirmation::descriptor()->name();

class EventEmissionConfirmation {
public:
    EventEmissionConfirmation(unsigned long long id);

    void process(std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
};

class EventEmissionConfirmationSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    std::unique_ptr<EventEmissionConfirmation>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif