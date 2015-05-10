/**
 * @file messageAcknowledgement.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/messageAcknowledgement.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {

MessageAcknowledgement::MessageAcknowledgement(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &messageAcknowledgementMsg = serverMessage->message_acknowledgement();
    m_streamId = messageAcknowledgementMsg.stream_id();
    m_sequenceNumber = messageAcknowledgementMsg.sequence_number();
}

uint64_t MessageAcknowledgement::streamId() const { return m_streamId; }

uint64_t MessageAcknowledgement::sequenceNumber() const
{
    return m_sequenceNumber;
}

std::string MessageAcknowledgement::toString() const
{
    std::stringstream stream;
    stream << "type: 'MessageAcknowledgement', stream ID: " << m_streamId
           << ", sequence number: " << m_sequenceNumber;
    return stream.str();
}

} // namespace messages
} // namespace one
