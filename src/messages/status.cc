/**
 * @file status.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/status.h"

#include "messages.pb.h"

#include <boost/bimap.hpp>
#include <boost/optional/optional_io.hpp>

#include <cassert>
#include <sstream>
#include <system_error>
#include <vector>

namespace {

using Translation = boost::bimap<one::clproto::Status_Code, std::errc>;

Translation createTranslation()
{
    using namespace one::clproto;

    const std::vector<Translation::value_type> pairs{
        {Status_Code_VOK, static_cast<std::errc>(0)},
        {Status_Code_VENOENT, std::errc::no_such_file_or_directory},
        {Status_Code_VEACCES, std::errc::permission_denied},
        {Status_Code_VEEXIST, std::errc::file_exists},
        {Status_Code_VEIO, std::errc::io_error},
        {Status_Code_VENOTSUP, std::errc::not_supported},
        {Status_Code_VENOTEMPTY, std::errc::directory_not_empty},
        {Status_Code_VEPERM, std::errc::operation_not_permitted},
        {Status_Code_VEINVAL, std::errc::invalid_argument},
        {Status_Code_VENOSPC, std::errc::no_space_on_device},
        {Status_Code_VEAGAIN, std::errc::resource_unavailable_try_again}};

    return {pairs.begin(), pairs.end()};
}

const Translation translation = createTranslation();
}

namespace one {
namespace messages {

Status::Status(std::error_code ec)
    : m_code{ec}
{
}

Status::Status(std::error_code ec, std::string desc)
    : m_code{ec}
    , m_description{std::move(desc)}
{
}

Status::Status(std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &statusMsg = serverMessage->status();
    std::tie(m_code, m_description) = translate(statusMsg);
}

std::error_code Status::code() const { return m_code; }

std::tuple<std::error_code, boost::optional<std::string>> Status::translate(
    const clproto::Status &status)
{
    auto searchResult = translation.left.find(status.code());
    const auto code = (searchResult == translation.left.end())
        ? std::errc::protocol_error
        : searchResult->second;

    if (status.has_description())
        return std::make_tuple(
            std::make_error_code(code), status.description());

    return std::make_tuple(
        std::make_error_code(code), boost::optional<std::string>{});
}

const boost::optional<std::string> &Status::description() const
{
    return m_description;
}

std::string Status::toString() const
{
    std::stringstream stream;
    stream << "type: 'Status', code: " << m_code
           << ", description: " << m_description;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Status::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto statusMsg = clientMsg->mutable_status();

    auto searchResult =
        translation.right.find(static_cast<std::errc>(m_code.value()));

    assert(searchResult != translation.right.end());
    statusMsg->set_code(searchResult->second);

    if (m_description)
        statusMsg->set_description(m_description.get());

    return clientMsg;
}

struct CodeHash {
    template <typename T> std::size_t operator()(T t) const
    {
        return static_cast<std::size_t>(t);
    }
};

} // namespace messages
} // namespace one
