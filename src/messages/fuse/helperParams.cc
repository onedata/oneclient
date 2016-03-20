/**
 * @file helperParams.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helperParams.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

HelperParams::HelperParams(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse(serverMessage)
{
    if (!serverMessage->fuse_response().has_helper_params())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "helper_params field missing"};

    deserialize(
        *serverMessage->mutable_fuse_response()->mutable_helper_params());
}

HelperParams::HelperParams(HelperParams::ProtocolMessage &message)
{
    deserialize(message);
}

HelperParams::HelperParams(
    std::string name, std::unordered_map<std::string, std::string> args)
    : m_name{std::move(name)}
    , m_args{std::move(args)}
{
}

std::string HelperParams::toString() const
{
    std::stringstream stream;
    stream << "type: 'HelperParams', name: '" << m_name << "', args: [";

    for (const auto &entry : m_args)
        stream << "'" << entry.first << "' -> '" << entry.second << "',";

    stream << "]";
    return stream.str();
}

void HelperParams::deserialize(ProtocolMessage &message)
{
    message.mutable_helper_name()->swap(m_name);

    for (auto &entry : *message.mutable_helper_args())
        entry.mutable_value()->swap(m_args[std::move(*entry.mutable_key())]);
}

} // namespace fuse
} // namespace messages
} // namespace one
