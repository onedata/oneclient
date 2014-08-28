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

namespace veil
{
namespace client
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

        if(const auto details = grAdapter.retrieveToken())
        {
            m_context.lock()->getConfig()->setTokenAuthDetails(details.get());
        }
        else
        {
            std::cout << "Authentication Code: ";
            std::string code;
            std::cin >> code;

            m_context.lock()->getConfig()->setTokenAuthDetails(
                        grAdapter.exchangeCode(code));
        }
    }
    catch(boost::system::error_code &ec)
    {
        throw AuthException{ec.message()};
    }
}

std::shared_ptr<communication::Communicator> AuthManager::createCommunicator(
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize) const
{
    const auto clusterUri = m_hostname+":"+std::to_string(m_port)+"/veilclient";

    if(m_certificateData)
        return communication::createWebsocketCommunicator(
                    dataPoolSize, metaPoolSize, clusterUri,
                    m_checkCertificate, m_certificateData);

    return communication::createWebsocketCommunicator(
                dataPoolSize, metaPoolSize, clusterUri, m_checkCertificate);
}


} // namespace client
} // namespace veil
