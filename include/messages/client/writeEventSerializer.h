/**
* @file writeEventSerializer.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_CLIENT_WRITE_EVENT_SERIALIZER_H
#define ONECLIENT_MESSAGES_CLIENT_WRITE_EVENT_SERIALIZER_H

#include "clientMessage.h"

namespace one {
namespace client {

/**
* The WriteEventSerializer class is responsible for creation of @c WriteEvent
* Protocol Buffers message.
*/
class WriteEventSerializer : public ClientMessageSerializer {
public:
    /**
    * @copydoc ClientMessageSerializer::serialize(const ClientMessage
    * &clientMessage)
    */
    virtual std::unique_ptr<ProtocolClientMessage>
    serialize(const ClientMessage &clientMessage) const override;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_CLIENT_WRITE_EVENT_SERIALIZER_H