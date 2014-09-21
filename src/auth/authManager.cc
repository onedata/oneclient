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

void AuthManager::authenticateWithCertificate(const bool debugGsi)
{
    GSIHandler gsiHandler{m_context, debugGsi};
    gsiHandler.validateProxyConfig();

    m_certificateData = gsiHandler.getCertData();
    m_hostname = gsiHandler.getClusterHostname(m_hostname);
}

void AuthManager::authenticateWithToken(std::string globalRegistryHostname,
                                        const unsigned int globalRegistryPort)
{
    try
    {
        GRAdapter grAdapter{
                    m_context,
                    std::move(globalRegistryHostname),
                    globalRegistryPort,
                    m_checkCertificate};

        if(auto details = grAdapter.retrieveToken())
        {
            m_authDetails = std::move(details.get());
        }
        else
        {
            std::cout << "Authorization Code: ";
            std::string code;
            std::cin >> code;

            m_authDetails = grAdapter.exchangeCode(code);
        }

        boost::lock_guard<boost::shared_mutex> guard{m_headersMutex};
        m_headers.emplace("global-user-id", m_authDetails.get().gruid());
        m_headers.emplace("authentication-secret", hashAndBase64(m_authDetails.get().accessToken()));

        m_grAdapter = std::move(grAdapter);
    }
    catch(boost::system::system_error &e)
    {
        throw AuthException{e.what()};
    }
}

std::shared_ptr<communication::Communicator> AuthManager::createCommunicator(
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize)
{
    std::function<decltype(m_headers)()> getHeadersFun = [this]{
        boost::shared_lock<boost::shared_mutex> lock{m_headersMutex};
        return m_headers;
    };

    if(m_certificateData)
        return communication::createWebsocketCommunicator(
                    dataPoolSize, metaPoolSize, m_hostname, m_port,
                    PROVIDER_CLIENT_ENDPOINT, m_checkCertificate,
                    getHeadersFun, m_certificateData);



    auto communicator = communication::createWebsocketCommunicator(
                dataPoolSize, metaPoolSize, m_hostname, m_port,
                PROVIDER_CLIENT_ENDPOINT, m_checkCertificate, getHeadersFun);

    scheduleRefresh(communicator);
    return communicator;
}

void AuthManager::scheduleRefresh(std::weak_ptr<communication::Communicator> communicator)
{
    const auto refreshIn = std::chrono::duration_cast<std::chrono::milliseconds>(
                m_authDetails.get().expirationTime() -
                std::chrono::system_clock::now()) * 4 / 5;

    m_context.lock()->scheduler()->schedule(
                refreshIn,
                std::bind(&AuthManager::refresh, this, communicator));
}

void AuthManager::refresh(std::weak_ptr<communication::Communicator> communicator)
{
    auto c = communicator.lock();
    if(!c)
        return;

    m_authDetails = m_grAdapter.get().refreshAccess(m_authDetails.get());
    scheduleRefresh(communicator);

    boost::lock_guard<boost::shared_mutex> guard{m_headersMutex};
    m_headers.emplace("global-user-id", m_authDetails.get().gruid());
    m_headers.emplace("authentication-secret", hashAndBase64(m_authDetails.get().accessToken()));

    c->recreate();
}

std::string AuthManager::hashAndBase64(const std::string &token) const
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
