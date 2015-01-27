/**
* @file eventEmissionRequest.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_EMISSION_REQUEST_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_EMISSION_REQUEST_H

#include "events.pb.h"
#include "eventMessage.h"

namespace one {
namespace client {
namespace events {

static const std::string EVENT_EMISSION_REQUEST_MESSAGE =
    one::clproto::events::EventEmissionRequest::descriptor()->name();

class EventEmissionRequest : public EventMessage {
public:
    EventEmissionRequest(unsigned long long id);

    virtual ~EventEmissionRequest() = default;

    virtual bool process(EventManager &manager) const override;

private:
    unsigned long long m_id;
};

class EventEmissionRequestSerializer : public EventMessageSerializer {
public:
    virtual ~EventEmissionRequestSerializer() = default;

    virtual std::unique_ptr<EventMessage>
    deserialize(const Message &message) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif