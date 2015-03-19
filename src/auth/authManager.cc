/**
 * @file authManager.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authException.h"
#include "auth/authManager.h"
#include "auth/grAdapter.h"
#include "communication/communicator.h"
#include "context.h"
#include "scheduler.h"
#include "options.h"
#include "system.h"

#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <openssl/sha.h>

#include <array>
#include <cassert>
#include <functional>
#include <iostream>
#include <unordered_map>

namespace one {
namespace client {

namespace auth {

AuthManager::AuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate)
    : m_context{std::move(context)}
    , m_hostname{std::move(defaultHostname)}
    , m_port{port}
    , m_checkCertificate{checkCertificate}
{
}

TokenAuthManager::TokenAuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate, std::string globalRegistryHostname,
    const unsigned int globalRegistryPort)
    : AuthManager{context, defaultHostname, port, checkCertificate}
    , m_grAdapter{System{}.clientName(), System{}.userDataDir(),
          std::move(globalRegistryHostname), globalRegistryPort,
          m_checkCertificate}
{
    try {
        if (auto details = m_grAdapter.retrieveToken()) {
            m_authDetails = std::move(details.get());
        }
        else {
            std::cout << "Authorization Code: ";
            std::string code;
            std::cin >> code;

            m_authDetails = m_grAdapter.exchangeCode(code);
        }

        if (m_context.lock()->options()->is_default_provider_hostname())
            m_hostname =
                "uuid_" + m_authDetails.gruid() + "." + defaultHostname;
    }
    catch (boost::system::system_error &e) {
        throw AuthException{e.what()};
    }
}

std::shared_ptr<communication::Communicator>
TokenAuthManager::createCommunicator(const unsigned int poolSize)
{
    /// @todo certificateData should be optional / set after construction
    auto communicator = std::make_shared<communication::Communicator>(poolSize,
        m_hostname, std::to_string(m_port), m_checkCertificate, nullptr);

    communicator->setHandshake(
        [this] {
            auto msg = std::make_unique<clproto::ClientMessage>();
            auto handshake = msg->mutable_handshake_request();
            auto token = handshake->mutable_token();

            handshake->set_session_id("testSessionId");
            token->set_value(m_authDetails.accessToken());
            return msg;
        },
        [](communication::ServerMessagePtr) { return true; });

    scheduleRefresh(communicator);
    return communicator;
}

void TokenAuthManager::scheduleRefresh(
    std::weak_ptr<communication::Communicator> communicator)
{
    const auto refreshIn =
        std::chrono::duration_cast<std::chrono::milliseconds>(
            m_authDetails.expirationTime() - std::chrono::system_clock::now()) *
        4 / 5;

    m_context.lock()->scheduler()->schedule(
        refreshIn, std::bind(&TokenAuthManager::refresh, this, communicator));
}

void TokenAuthManager::refresh(
    std::weak_ptr<communication::Communicator> communicator)
{
    auto c = communicator.lock();
    if (!c)
        return;

    m_authDetails = m_grAdapter.refreshAccess(m_authDetails);
    scheduleRefresh(communicator);
}

} // namespace auth
} // namespace client
} // namespace one
