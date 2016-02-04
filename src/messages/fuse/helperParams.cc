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

    auto &message =
        *serverMessage->mutable_fuse_response()->mutable_helper_params();
    m_name.swap(*message.mutable_helper_name());

    for (auto &entry : *message.mutable_helper_args())
        m_args[std::move(*entry.mutable_key())].swap(*entry.mutable_value());
}

HelperParams::HelperParams(const HelperParams::ProtocolMessage &message)
{
    m_name = message.helper_name();
    for (const auto &entry : message.helper_args())
        m_args.emplace(entry.key(), entry.value());
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

} // namespace fuse
} // namespace messages
} // namespace one
