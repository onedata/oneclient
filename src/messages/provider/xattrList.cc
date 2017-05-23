/**
 * @file xattrList.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "xattrList.h"
#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace provider {

XAttrList::XAttrList(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : ProviderResponse{serverMessage}
{
    if (!serverMessage->provider_response().has_xattr_list())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "xattr_list field missing"};

    auto xattrList =
        serverMessage->mutable_provider_response()->mutable_xattr_list();

    for (int i = 0; i < xattrList->names_size(); i++) {
        m_xattrNames.emplace_back(xattrList->names(i));
    }
}

const std::vector<std::string> &XAttrList::xattrNames() const
{
    return m_xattrNames;
}

std::string XAttrList::toString() const
{
    std::stringstream stream;
    stream << "type: 'XAttrList', extended attribute names: [";

    for (const auto &xattrName : m_xattrNames)
        stream << xattrName << ", ";

    stream << "]";

    return stream.str();
}

} // namespace provider
} // namespace messages
} // namespace one
