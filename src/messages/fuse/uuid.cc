/**
 * @file uuid.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "uuid.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

Uuid::Uuid(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_uuid()) {
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "uuid field missing"};
    }

    auto *uuid = serverMessage->mutable_fuse_response()->mutable_uuid();

    m_uuid = uuid->uuid();
}

const std::string &Uuid::uuid() const { return m_uuid; }

std::string Uuid::toString() const
{
    std::stringstream stream;
    stream << "type: 'Uuid', uuid: " << m_uuid;

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
