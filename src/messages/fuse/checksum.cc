/**
 * @file checksum.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "checksum.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Checksum::Checksum(std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    if (!serverMessage->fuse_response().has_checksum())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "checksum field missing"};

    auto &message = *serverMessage->mutable_fuse_response()->mutable_checksum();

    message.mutable_value()->swap(m_value);
}

const std::string &Checksum::value() const { return m_value; }

std::string Checksum::toString() const
{
    std::stringstream ss;
    ss << "type: 'Checksum', value: '" << m_value;
    return ss.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
