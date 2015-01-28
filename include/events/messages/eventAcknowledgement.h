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

/**
* Name of an event acknowledgement message.
*/
static const std::string EVENT_ACKNOWLEDGEMENT_MESSAGE =
    one::clproto::events::EventAcknowledgement::descriptor()->name();

/**
* The EventAcknowledgement class represents a message sent by the server to
* inform about a sequence number of a last successfully processed event.
*/
class EventAcknowledgement {
    friend std::ostream &
    operator<<(std::ostream &, const EventAcknowledgement &acknowledgement);

public:
    /**
    * Constructor.
    * @param sequenceNumber Sequence number of a last successfully processed
    * event.
    */
    EventAcknowledgement(unsigned long long sequenceNumber);

    /**
    * Processes an event acknowledgement message by removing from an event
    * buffer all events with a sequence number less than or equal to @p
    * sequenceNumber.
    * @param buffer Weak pointer to an event buffer.
    */
    void process(std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_sequenceNumber;
};

/**
* The EventAcknowledgementSerializer class is responsible for deserialization of
* the EventAcknowledgement messages.
*/
class EventAcknowledgementSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Deserializes the EventAcknowledgement message.
    * @param message Message to deserialize.
    */
    std::unique_ptr<EventAcknowledgement>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_EVENT_ACKNOWLEDGEMENT_H