/**
 * @file authManager.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authManager.h"
#include "auth/authException.h"
#include "auth/gsiHandler.h"
#include "auth/tokenHandler.h"
#include "communication/cert/certificateData.h"
#include "communication/communicator.h"
#include "communication/persistentConnection.h"
#include "context.h"
#include "environment.h"
#include "options.h"
#include "scheduler.h"

#include "messages/token.h"

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

CertificateAuthManager::CertificateAuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate, const bool debugGsi)
    : AuthManager{context, defaultHostname, port, checkCertificate}
{
    GSIHandler gsiHandler{m_context, debugGsi};
    gsiHandler.validateProxyConfig();

    m_certificateData = gsiHandler.getCertData();
    m_hostname = gsiHandler.getClusterHostname(m_hostname);
}

std::tuple<std::shared_ptr<communication::Communicator>, std::future<void>>
CertificateAuthManager::createCommunicator(const unsigned int poolSize,
    std::string sessionId,
    std::function<std::error_code(messages::HandshakeResponse)>
        onHandshakeResponse)
{
    auto communicator =
        std::make_shared<communication::Communicator>(poolSize, m_hostname,
            m_port, m_checkCertificate, communication::createConnection);

    communicator->setCertificateData(m_certificateData);

    one::messages::HandshakeRequest handshake{std::move(sessionId)};
    auto future = communicator->setHandshake(
        [=] { return handshake; }, std::move(onHandshakeResponse));

    return std::forward_as_tuple(std::move(communicator), std::move(future));
}

TokenAuthManager::TokenAuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate)
    : AuthManager{context, defaultHostname, port, checkCertificate}
    , m_tokenHandler{*context.lock()->options(), m_environment.userDataDir(),
          "TODO:ProviderId"}
{
}

TokenAuthManager::~TokenAuthManager() { m_cancelRefresh(); }

std::tuple<std::shared_ptr<communication::Communicator>, std::future<void>>
TokenAuthManager::createCommunicator(const unsigned int poolSize,
    std::string sessionId,
    std::function<std::error_code(messages::HandshakeResponse)>
        onHandshakeResponse)
{
    m_cancelRefresh();

    auto communicator =
        std::make_shared<communication::Communicator>(poolSize, m_hostname,
            m_port, m_checkCertificate, communication::createConnection);

    auto future = communicator->setHandshake(
        [=] {
            one::messages::HandshakeRequest handshake{
                sessionId, m_tokenHandler.restrictedToken()};

            return handshake;
        },
        std::move(onHandshakeResponse));

    scheduleRefresh(RESTRICTED_MACAROON_REFRESH);

    return std::forward_as_tuple(std::move(communicator), std::move(future));
}

void TokenAuthManager::refreshToken()
{
    LOG(INFO) << "Sending a refreshed token";
    auto future = m_context.lock()->communicator()->send(
        one::messages::Token{m_tokenHandler.refreshRestrictedToken()});

    try {
        communication::wait(future);
        scheduleRefresh(RESTRICTED_MACAROON_REFRESH);
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Sending a refreshed token failed with error: "
                     << e.what();

        scheduleRefresh(FAILED_TOKEN_REFRESH_RETRY);
    }
}

void TokenAuthManager::scheduleRefresh(const std::chrono::seconds after)
{
    LOG(INFO) << "Scheduling next token refresh in "
              << std::chrono::duration_cast<std::chrono::seconds>(after).count()
              << " seconds";

    m_cancelRefresh = m_context.lock()->scheduler()->schedule(
        after, std::bind(&TokenAuthManager::refreshToken, this));
}

} // namespace auth
} // namespace client
} // namespace one
