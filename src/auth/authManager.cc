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

#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <openssl/sha.h>

#include <array>
#include <cassert>
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
        GRAdapter grAdapter{m_context,
                    std::move(globalRegistryHostname),
                    globalRegistryPort,
                    m_checkCertificate};

        if(auto details = grAdapter.retrieveToken())
        {
            m_tokenAuthDetails = std::move(details);
        }
        else
        {
            std::cout << "Authentication Code: ";
            std::string code;
            std::cin >> code;

            m_tokenAuthDetails = grAdapter.exchangeCode(code);
        }
    }
    catch(boost::system::system_error &e)
    {
        throw AuthException{e.what()};
    }
}

std::shared_ptr<communication::Communicator> AuthManager::createCommunicator(
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize) const
{
    std::unordered_map<std::string, std::string> authHeaders;

    if(m_certificateData)
        return communication::createWebsocketCommunicator(
                    dataPoolSize, metaPoolSize, m_hostname, m_port,
                    PROVIDER_CLIENT_ENDPOINT, m_checkCertificate,
                    authHeaders, m_certificateData);

    assert(m_tokenAuthDetails);
    const auto &authDetails = m_tokenAuthDetails.get();

    authHeaders.emplace("global-user-id", authDetails.gruid());
    authHeaders.emplace("authentication-secret", hashAndBase64(authDetails.accessToken()));

    return communication::createWebsocketCommunicator(
                dataPoolSize, metaPoolSize, m_hostname, m_port,
                PROVIDER_CLIENT_ENDPOINT, m_checkCertificate, authHeaders);
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
