/**
 * @file processingStatus.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/processingStatus.h"

#include "logging.h"

namespace one {
namespace messages {

ProcessingStatus::ProcessingStatus(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    if (!serverMessage->processing_status().has_code()) {
        LOG(ERROR) << "Invalid ProcessingStatus - missing code field";
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "code field missing"};
    }

    m_code = serverMessage->processing_status().code();
}

one::clproto::ProcessingCode ProcessingStatus::code() const { return m_code; }

std::string ProcessingStatus::toString() const
{
    return "type: 'ProcessingStatus'";
}

} // namespace messages
} // namespace one
