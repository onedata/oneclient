/**
 * @file status.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/status.h"

#include "client_messages.pb.h"
#include "server_messages.pb.h"

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
    switch (statusMsg.code()) {
        case one::clproto::Status_Code_VOK:
            m_code = Status::Code::ok;
        case one::clproto::Status_Code_VENOENT:
            m_code = Status::Code::enoent;
        case one::clproto::Status_Code_VEACCES:
            m_code = Status::Code::eacces;
        case one::clproto::Status_Code_VEEXIST:
            m_code = Status::Code::eexist;
        case one::clproto::Status_Code_VEIO:
            m_code = Status::Code::eio;
        case one::clproto::Status_Code_VENOTSUP:
            m_code = Status::Code::enotsup;
        case one::clproto::Status_Code_VENOTEMPTY:
            m_code = Status::Code::enotempty;
        case one::clproto::Status_Code_VEPERM:
            m_code = Status::Code::eperm;
        case one::clproto::Status_Code_VEINVAL:
            m_code = Status::Code::einval;
        case one::clproto::Status_Code_VEDQUOT:
            m_code = Status::Code::edquot;
        case one::clproto::Status_Code_VENOATTR:
            m_code = Status::Code::enoattr;
        case one::clproto::Status_Code_VECOMM:
            m_code = Status::Code::ecomm;
        default:
            m_code = Status::Code::eremoteio;
    }
    if (statusMsg.has_description())
        m_description = statusMsg.description();
}

std::unique_ptr<ProtocolClientMessage> Status::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto statusMsg = clientMsg->mutable_status();

    switch (m_code) {
        case Status::Code::ok:
            statusMsg->set_code(one::clproto::Status_Code_VOK);
        case Status::Code::enoent:
            statusMsg->set_code(one::clproto::Status_Code_VENOENT);
        case Status::Code::eacces:
            statusMsg->set_code(one::clproto::Status_Code_VEACCES);
        case Status::Code::eexist:
            statusMsg->set_code(one::clproto::Status_Code_VEEXIST);
        case Status::Code::eio:
            statusMsg->set_code(one::clproto::Status_Code_VEIO);
        case Status::Code::enotsup:
            statusMsg->set_code(one::clproto::Status_Code_VENOTSUP);
        case Status::Code::enotempty:
            statusMsg->set_code(one::clproto::Status_Code_VENOTEMPTY);
        case Status::Code::eperm:
            statusMsg->set_code(one::clproto::Status_Code_VEPERM);
        case Status::Code::einval:
            statusMsg->set_code(one::clproto::Status_Code_VEINVAL);
        case Status::Code::edquot:
            statusMsg->set_code(one::clproto::Status_Code_VEDQUOT);
        case Status::Code::enoattr:
            statusMsg->set_code(one::clproto::Status_Code_VENOATTR);
        case Status::Code::ecomm:
            statusMsg->set_code(one::clproto::Status_Code_VECOMM);
        default:
            statusMsg->set_code(one::clproto::Status_Code_VEREMOTEIO);
    }

    if (m_description)
        statusMsg->set_description(m_description.get());

    return clientMsg;
}

Status::Code Status::code() const { return m_code; }

const boost::optional<std::string> &Status::description() const
{
    return m_description;
}

} // namespace messages
} // namespace one
