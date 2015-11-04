/**
 * @file declarations.h
 * This file contains common declarations for communication classes.
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_DECLARATIONS_H
#define HELPERS_COMMUNICATION_DECLARATIONS_H

// ClientMessage and ServerMessage are not forward-declared, because it's to be
// used from header-only communication classes.
#include "messages.pb.h"

#include <chrono>
#include <memory>

namespace one {
namespace communication {

constexpr int DEFAULT_RETRY_NUMBER = 2;
constexpr int STREAM_MSG_ACK_WINDOW = 100;
constexpr std::chrono::seconds STREAM_MSG_REQ_WINDOW{30};

using ServerMessagePtr = std::unique_ptr<clproto::ServerMessage>;
using ClientMessagePtr = std::unique_ptr<clproto::ClientMessage>;

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_DECLARATIONS_H
