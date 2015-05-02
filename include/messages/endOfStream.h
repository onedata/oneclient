/**
* @file endOfStream.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_END_OF_STREAM_H
#define ONECLIENT_MESSAGES_END_OF_STREAM_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
* The EndOfStream class represents a message that is sent by the client to close
* message stream.
*/
class EndOfStream : public ClientMessage {
public:
    virtual std::string toString() const override;

    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_END_OF_STREAM_H
