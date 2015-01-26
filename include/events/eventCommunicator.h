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

class EventBuffer;

class EventCommunicator {
public:
    EventCommunicator(std::weak_ptr<EventBuffer> buffer,
                      std::shared_ptr<Context> context);

private:
    void send(const google::protobuf::Message &message) const;

    long long m_sequence_number;
    std::weak_ptr<EventBuffer> m_buffer;
    std::shared_ptr<Context> m_context;
};

} // namespace events
} // namespace client
} // namespace one

#endif