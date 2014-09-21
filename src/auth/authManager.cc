/**
 * @file authManager.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "auth/authException.h"
#include "auth/authManager.h"
#include "auth/grAdapter.h"
#include "auth/gsiHandler.h"
#include "communication/certificateData.h"
#include "communication/communicator.h"
#include "config.h"
#include "context.h"
#include "make_unique.h"
#include "scheduler.h"

#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <openssl/sha.h>

#include <array>
#include <cassert>
#include <functional>
#include <unordered_map>

namespace veil
{
namespace client
{

namespace auth
{

AuthManager::AuthManager(std::weak_ptr<Context> context,
                         std::string defaultHostname,
                         const unsigned int port,
                         const bool checkCertificate)
    : m_context{std::move(context)}
    , m_hostname{std::move(defaultHostname)}
    , m_port{port}
    , m_checkCertificate{checkCertificate}
{
}

CertificateAuthManager::CertificateAuthManager(std::weak_ptr<Context> context,
                                               std::string defaultHostname,
                                               const unsigned int port,
                                               const bool checkCertificate,
                                               const bool debugGsi)
    : AuthManager{context, defaultHostname, port, checkCertificate}
{
    GSIHandler gsiHandler{m_context, debugGsi};
    gsiHandler.validateProxyConfig();

    m_certificateData = gsiHandler.getCertData();
    m_hostname = gsiHandler.getClusterHostname(m_hostname);
}

std::shared_ptr<communication::Communicator> CertificateAuthManager::createCommunicator(
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize)
{
    const auto getHeadersFun = []{
        return std::unordered_map<std::string, std::string>{};
    };

    return communication::createWebsocketCommunicator(
                dataPoolSize, metaPoolSize, m_hostname, m_port,
                PROVIDER_CLIENT_ENDPOINT, m_checkCertificate,
                std::move(getHeadersFun), m_certificateData);
}

TokenAuthManager::TokenAuthManager(std::weak_ptr<Context> context,
                                   std::string defaultHostname,
                                   const unsigned int port,
                                   const bool checkCertificate,
                                   std::string globalRegistryHostname,
                                   const unsigned int globalRegistryPort)
    : AuthManager{context, defaultHostname, port, checkCertificate}
    , m_grAdapter{m_context, std::move(globalRegistryHostname), globalRegistryPort, m_checkCertificate}
{
    try
    {
        if(auto details = m_grAdapter.retrieveToken())
        {
            m_authDetails = std::move(details.get());
        }
        else
        {
            std::cout << "Authorization Code: ";
            std::string code;
            std::cin >> code;

            m_authDetails = m_grAdapter.exchangeCode(code);
        }

        boost::lock_guard<boost::shared_mutex> guard{m_headersMutex};
        m_headers.emplace("global-user-id", m_authDetails.gruid());
        m_headers.emplace("authentication-secret", hashAndBase64(m_authDetails.accessToken()));
    }
    catch(boost::system::system_error &e)
    {
        throw AuthException{e.what()};
    }
}

std::shared_ptr<communication::Communicator> TokenAuthManager::createCommunicator(
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize)
{
    auto getHeadersFun = [this]{
        boost::shared_lock<boost::shared_mutex> lock{m_headersMutex};
        return m_headers;
    };

    auto communicator = communication::createWebsocketCommunicator(
                dataPoolSize, metaPoolSize, m_hostname, m_port,
                PROVIDER_CLIENT_ENDPOINT, m_checkCertificate,
                std::move(getHeadersFun));

    scheduleRefresh(communicator);
    return communicator;
}


void TokenAuthManager::scheduleRefresh(std::weak_ptr<communication::Communicator> communicator)
{
    const auto refreshIn = std::chrono::duration_cast<std::chrono::milliseconds>(
                m_authDetails.expirationTime() - std::chrono::system_clock::now()) * 4 / 5;

    m_context.lock()->scheduler()->schedule(
                refreshIn, std::bind(&TokenAuthManager::refresh, this, communicator));
}

void TokenAuthManager::refresh(std::weak_ptr<communication::Communicator> communicator)
{
    auto c = communicator.lock();
    if(!c)
        return;

    m_authDetails = m_grAdapter.refreshAccess(m_authDetails);
    scheduleRefresh(communicator);

    boost::lock_guard<boost::shared_mutex> guard{m_headersMutex};
    m_headers.emplace("global-user-id", m_authDetails.gruid());
    m_headers.emplace("authentication-secret", hashAndBase64(m_authDetails.accessToken()));

    c->recreate();
}

std::string TokenAuthManager::hashAndBase64(const std::string &token) const
{
    std::array<unsigned char, SHA512_DIGEST_LENGTH> digest;

    SHA512(reinterpret_cast<const unsigned char*>(token.c_str()),
           token.length(), digest.data());

    using base = boost::archive::iterators::base64_from_binary<
        boost::archive::iterators::transform_width<decltype(digest)::const_iterator, 6, 8>>;

    const std::string base64hash{base{digest.begin()}, base{digest.end()}};
    const std::string padding(3 - (SHA512_DIGEST_LENGTH % 3), '=');

    return base64hash + padding;
}


} // namespace auth
} // namespace client
} // namespace veil
