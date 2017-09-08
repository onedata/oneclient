/**
 * @file messageStreamReset.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/messageStreamReset.h"

#include "messages.pb.h"

namespace one {
namespace messages {

MessageStreamReset::MessageStreamReset(std::unique_ptr<ProtocolServerMessage>)
{
}

std::string MessageStreamReset::toString() const
{
    return "type : 'MessageStreamReset'";
}

} // namespace messages
} // namespace one
