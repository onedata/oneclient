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

#include <memory>

namespace one {
namespace client {
namespace events {

class EventBuffer;
class EventCommunicator;

static const std::string EVENT_EMISSION_REQUEST_MESSAGE =
    one::clproto::events::EventEmissionRequest::descriptor()->name();

class EventEmissionRequest {
public:
    EventEmissionRequest(unsigned long long id);

    void process(std::weak_ptr<EventBuffer> buffer,
                 std::weak_ptr<EventCommunicator> communicator) const;

private:
    unsigned long long m_id;
};

class EventEmissionRequestSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    std::unique_ptr<EventEmissionRequest>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif