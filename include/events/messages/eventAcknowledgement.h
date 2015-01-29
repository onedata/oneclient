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
* Name of an @c EventAcknowledgement message.
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
    * @param seqNum Sequence number of a last successfully processed
    * event.
    */
    EventAcknowledgement(unsigned long long seqNum);

    /**
    * Processes an event acknowledgement message by removing from an event
    * buffer all events with a sequence number less than or equal to @p
    * seqNum.
    * @param buffer An @c EventBuffer instance.
    */
    void process(std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_seqNum;
};

/**
* The EventAcknowledgementSerializer class is responsible for deserialization of
* the @c EventAcknowledgement messages.
*/
class EventAcknowledgementSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Deserializes the @c EventAcknowledgement message.
    * @param message Message to deserialize.
    * @return Returns deserialized @c EventAcknowledgement instance.
    */
    std::unique_ptr<EventAcknowledgement>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_EVENT_ACKNOWLEDGEMENT_H