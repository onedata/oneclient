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
#include "eventMessage.h"

namespace one {
namespace client {
namespace events {

static const std::string EVENT_EMISSION_CONFIRMATION_MESSAGE =
    one::clproto::events::EventEmissionConfirmation::descriptor()->name();

class EventEmissionConfirmation : public EventMessage {
public:
    EventEmissionConfirmation(long long id);

    virtual ~EventEmissionConfirmation() = default;

    virtual bool process(EventManager &manager) const override;

private:
    long long m_id;
};

class EventEmissionConfirmationSerializer : public EventMessageSerializer {
public:
    virtual ~EventEmissionConfirmationSerializer() = default;

    virtual std::unique_ptr<EventMessage>
    deserialize(const Message &message) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif