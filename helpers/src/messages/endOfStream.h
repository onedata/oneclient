/**
* @file endOfStream.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_END_OF_STREAM_H
#define HELPERS_MESSAGES_END_OF_STREAM_H

#include "messages/clientMessage.h"

#include <memory>

namespace one {
namespace messages {

/**
* The EndOfStream class represents a message that is sent by the client to close
* message stream.
*/
class EndOfStream : public ClientMessage {
public:
    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_END_OF_STREAM_H
