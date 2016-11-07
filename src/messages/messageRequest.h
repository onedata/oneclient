/**
 * @file handshakeResponse.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_MESSAGE_REQUEST_H
#define HELPERS_MESSAGES_MESSAGE_REQUEST_H

#include "serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The MessageRequest class represents a message that is sent by the server to
 * request retransmission of stream messages.
 */
class MessageRequest : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * MessageRequest counterpart.
     */
    MessageRequest(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return ID of stream requested messages belong to.
     */
    uint64_t streamId() const;

    /**
     * @return Lower bound (inclusive) of sequence numbers of requested
     * messages.
     */
    uint64_t lowerSequenceNumber() const;

    /**
     * @return Upper bound (inclusive) of sequence numbers of requested
     * messages.
     */
    uint64_t upperSequenceNumber() const;

    virtual std::string toString() const override;

private:
    uint64_t m_streamId;
    uint64_t m_lowerSequenceNumber;
    uint64_t m_upperSequenceNumber;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_MESSAGE_REQUEST_H
