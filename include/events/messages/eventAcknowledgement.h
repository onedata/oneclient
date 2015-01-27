/**
* @file eventAcknowledgement.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_ACKNOWLEDGEMENT_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_ACKNOWLEDGEMENT_H

#include "events.pb.h"

#include <memory>

namespace one {

namespace clproto {
namespace communication_protocol {
class Answer;
}
}

namespace client {
namespace events {

class EventBuffer;

static const std::string EVENT_EMISSION_CONFIRMATION_MESSAGE =
    one::clproto::events::EventAcknowledgement::descriptor()->name();

class EventAcknowledgement {
public:
    EventAcknowledgement(unsigned long long id);

    void process(std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
};

class EventAcknowledgementSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    std::unique_ptr<EventAcknowledgement>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif