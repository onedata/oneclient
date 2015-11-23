/**
 * @file endOfStream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/endOfStream.h"

#include "messages.pb.h"

namespace one {
namespace messages {

std::string EndOfStream::toString() const { return "type: 'EndOfStream'"; }

std::unique_ptr<ProtocolClientMessage> EndOfStream::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_end_of_stream();

    return clientMsg;
}

} // namespace messages
} // namespace one
