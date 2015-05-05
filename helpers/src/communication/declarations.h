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
#include "client_messages.pb.h"
#include "server_messages.pb.h"

#include <memory>

namespace one {
namespace communication {

constexpr int DEFAULT_RETRY_NUMBER = 2;

using ServerMessagePtr = std::unique_ptr<clproto::ServerMessage>;
using ClientMessagePtr = std::unique_ptr<clproto::ClientMessage>;

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_DECLARATIONS_H