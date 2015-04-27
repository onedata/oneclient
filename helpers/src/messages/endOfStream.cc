/**
 * @file endOfStream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/endOfStream.h"

#include "client_messages.pb.h"

namespace one {
namespace messages {

std::unique_ptr<ProtocolClientMessage> EndOfStream::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_end_of_stream();

    return clientMsg;
}

} // namespace messages
} // namespace one
