/**
 * @file messageAcknowledgement.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_MESSAGE_ACKNOWLEDGEMENT_H
#define HELPERS_MESSAGES_MESSAGE_ACKNOWLEDGEMENT_H

#include "serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The MessageAcknowledgement class represents a message that is sent by the
 * server to inform about sequence number of last successfully processed stream
 * message.
 */
class MessageAcknowledgement : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * MessageAcknowledgement counterpart.
     */
    MessageAcknowledgement(
        std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return ID of stream acknowledged message belongs to.
     */
    uint64_t streamId() const;

    /**
     * @return Sequence number of last successfully processed message from the
     * stream.
     */
    uint64_t sequenceNumber() const;

    virtual std::string toString() const override;

private:
    uint64_t m_streamId;
    uint64_t m_sequenceNumber;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_MESSAGE_ACKNOWLEDGEMENT_H
