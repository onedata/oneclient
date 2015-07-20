/**
 * @file authManager.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "environment.h"
#include "options.h"
#include "auth/authException.h"
#include "auth/authManager.h"
#include "auth/grAdapter.h"
#include "auth/gsiHandler.h"
#include "communication/cert/certificateData.h"
#include "communication/persistentConnection.h"
#include "communication/communicator.h"

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
    const bool checkCertificate, std::string globalRegistryHostname,
    const unsigned int globalRegistryPort)
    : AuthManager{context, defaultHostname, port, checkCertificate}
    , m_grAdapter{m_environment.clientName(), m_environment.userDataDir(),
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
            m_hostname = "uid_" + m_authDetails.gruid() + "." + m_hostname;
    }
    catch (boost::system::system_error &e) {
        throw AuthException{e.what()};
    }
}

std::tuple<std::shared_ptr<communication::Communicator>, std::future<void>>
TokenAuthManager::createCommunicator(const unsigned int poolSize,
    std::string sessionId,
    std::function<std::error_code(messages::HandshakeResponse)>
        onHandshakeResponse)
{
    auto communicator =
        std::make_shared<communication::Communicator>(poolSize, m_hostname,
            m_port, m_checkCertificate, communication::createConnection);

    one::messages::HandshakeRequest handshake{
        sessionId, m_authDetails.accessToken()};

    auto future = communicator->setHandshake(
        [=] { return handshake; }, std::move(onHandshakeResponse));

    /// @todo Refreshing the token

    return std::forward_as_tuple(std::move(communicator), std::move(future));
}

} // namespace auth
} // namespace client
} // namespace one
