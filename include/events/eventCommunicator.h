/**
* @file eventCommunicator.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H
#define ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H

#include <google/protobuf/message.h>

#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

/**
* The EventCommunicator class is responsible for sending event messages to the
* server.
*/
class EventCommunicator {
public:
    /**
    * Constructor.
    * @param context An @c Context instance.
    */
    EventCommunicator(std::shared_ptr<Context> context);

    /**
    * Sends event message to the server.
    * @param message Message to be sent.
    * @return Returns @c true in case of successful emission or @c false
    * otherwise.
    */
    bool send(const google::protobuf::Message &message) const;

private:
    std::shared_ptr<Context> m_context;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H