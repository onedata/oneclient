/**
* @file eventRequest.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_REQUEST_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_REQUEST_H

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
class EventCommunicator;

/**
* Name of an @c EventRequest message.
*/
static const std::string EVENT_REQUEST_MESSAGE =
    one::clproto::events::EventRequest::descriptor()->name();

/**
* The EventRequest class represents a message sent by the server to
* request emission of an event.
*/
class EventRequest {
    friend std::ostream &operator<<(std::ostream &,
                                    const EventRequest &request);

public:
    /**
    * Constructor.
    * @param seqNum Sequence number of a requested event.
    */
    EventRequest(unsigned long long seqNum);

    /**
    * Processes an event request message by retrieving an event from an event
    * buffer and resend it.
    * @param buffer An @c EventBuffer instance.
    * @param communicator An @c EventCommunicator instance.
    */
    void process(std::weak_ptr<EventBuffer> buffer,
                 std::weak_ptr<EventCommunicator> communicator) const;

private:
    unsigned long long m_seqNum;
};

/**
* The EventRequestSerializer class is responsible for deserialization of the
* @c EventRequest messages.
*/
class EventRequestSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Deserializes the @c EventRequest message.
    * @param message Message to deserialize.
    * @return Returns deserialized @c EventRequest instance.
    */
    std::unique_ptr<EventRequest> deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_EVENT_REQUEST_H