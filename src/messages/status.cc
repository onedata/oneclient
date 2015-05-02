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

#include <sstream>

namespace {

using Translation =
    boost::bimap<one::clproto::Status_Code, one::messages::Status::Code>;

Translation createTranslation()
{

    Translation translation;

    translation.insert(Translation::value_type(
        one::clproto::Status_Code_VOK, one::messages::Status::Code::ok));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VENOENT,
            one::messages::Status::Code::enoent));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VEACCES,
            one::messages::Status::Code::eacces));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VEEXIST,
            one::messages::Status::Code::eexist));
    translation.insert(Translation::value_type(
        one::clproto::Status_Code_VEIO, one::messages::Status::Code::eio));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VENOTSUP,
            one::messages::Status::Code::enotsup));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VENOTEMPTY,
            one::messages::Status::Code::enotempty));
    translation.insert(Translation::value_type(
        one::clproto::Status_Code_VEPERM, one::messages::Status::Code::eperm));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VEINVAL,
            one::messages::Status::Code::einval));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VEDQUOT,
            one::messages::Status::Code::edquot));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VENOATTR,
            one::messages::Status::Code::enoattr));
    translation.insert(Translation::value_type(
        one::clproto::Status_Code_VECOMM, one::messages::Status::Code::ecomm));
    translation.insert(
        Translation::value_type(one::clproto::Status_Code_VEREMOTEIO,
            one::messages::Status::Code::eremoteio));

    return translation;
}

const Translation translation = createTranslation();
}

namespace one {
namespace messages {

Status::Status(Code code)
    : m_code{code}
{
}

Status::Status(Code code, std::string description)
    : m_code{code}
    , m_description{std::move(description)}
{
}

Status::Status(std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &statusMsg = serverMessage->status();

    auto searchResult = translation.left.find(statusMsg.code());
    if (searchResult != translation.left.end()) {
        m_code = searchResult->second;
    }
    else {
        m_code = Code::eremoteio;
    }

    if (statusMsg.has_description())
        m_description = statusMsg.description();
}

Status::Code Status::code() const { return m_code; }

const boost::optional<std::string> &Status::description() const
{
    return m_description;
}

std::string Status::toString() const
{
    std::stringstream stream;
    stream << "type: 'Status', code: " << m_code << ", description: ";
    if (m_description)
        stream << "'" << m_description.get() << "'";
    else
        stream << "'undefined'";
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Status::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto statusMsg = clientMsg->mutable_status();

    auto searchResult = translation.right.find(m_code);
    if (searchResult != translation.right.end()) {
        statusMsg->set_code(searchResult->second);
    }
    else {
        statusMsg->set_code(one::clproto::Status_Code_VEREMOTEIO);
    }

    if (m_description)
        statusMsg->set_description(m_description.get());

    return clientMsg;
}

} // namespace messages
} // namespace one
