/**
 * @file configuration.h
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authManager.h"
#include "communication/communicator.h"
#include "messages/configuration.h"

namespace one {
namespace client {

std::string generateSessionId();

std::shared_ptr<communication::Communicator> handshake(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context);

std::shared_ptr<auth::AuthManager> getAuthManager(
    std::shared_ptr<Context> context);

std::shared_ptr<messages::Configuration> getConfiguration(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context, bool quiet = false);

std::shared_ptr<communication::Communicator> getCommunicator(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context);
} // namespace client
} // namespace one
