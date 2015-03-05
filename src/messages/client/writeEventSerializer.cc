/**
* @file writeEventSerializer.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "messages/client/writeEventSerializer.h"

namespace one {
namespace client {

std::unique_ptr<ClientMessageSerializer::ProtocolClientMessage>
WriteEventSerializer::serialize(const ClientMessage &clientMessage) const
{
    // @todo Complete implementation after integration with new protocol,
    // consider moving 'messages' directory do helpers project
    return nullptr;
}

} // namespace client
} // namespace one
