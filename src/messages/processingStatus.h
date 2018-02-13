/**
 * @file processingStatus.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PROCESSING_STATUS_H
#define HELPERS_MESSAGES_PROCESSING_STATUS_H

#include "serverMessage.h"

#include "messages.pb.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The ProcessingStatus class represents a message that is sent by the server to
 * update on ongoing request processing on the server side.
 */
class ProcessingStatus : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * ProcessingStatus counterpart.
     */
    ProcessingStatus(std::unique_ptr<ProtocolServerMessage> serverMessage);

    one::clproto::ProcessingCode code() const;

    virtual std::string toString() const override;

private:
    one::clproto::ProcessingCode m_code;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROCESSING_CODE_H
