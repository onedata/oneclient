/**
 * @file messageRequest.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/messageRequest.h"

#include "messages.pb.h"

namespace one {
namespace messages {

MessageRequest::MessageRequest(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &messageRequestMsg = serverMessage->message_request();
    m_streamId = messageRequestMsg.stream_id();
    m_lowerSequenceNumber = messageRequestMsg.lower_sequence_number();
    m_upperSequenceNumber = messageRequestMsg.upper_sequence_number();
}

uint64_t MessageRequest::streamId() const { return m_streamId; }

uint64_t MessageRequest::lowerSequenceNumber() const
{
    return m_lowerSequenceNumber;
}

uint64_t MessageRequest::upperSequenceNumber() const
{
    return m_upperSequenceNumber;
}

} // namespace messages
} // namespace one
