/**
* @file readEventSerializer.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_CLIENT_READ_EVENT_SERIALIZER_H
#define ONECLIENT_MESSAGES_CLIENT_READ_EVENT_SERIALIZER_H

#include "messages/client/clientMessage.h"

namespace one {
namespace client {

/**
* The ReadEventSerializer class is responsible for creation of @c ReadEvent
* Protocol Buffers message.
*/
class ReadEventSerializer : public messages::client::ClientMessageSerializer {
public:
    virtual std::unique_ptr<ProtocolClientMessage> serialize(
        const messages::client::ClientMessage &clientMessage) const override;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_CLIENT_READ_EVENT_SERIALIZER_H
