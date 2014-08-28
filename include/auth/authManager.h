/**
 * @file authManager.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_AUTH_MANAGER_H
#define VEILCLIENT_AUTH_MANAGER_H


#include "auth/tokenAuthDetails.h"

#include <memory>

namespace veil
{

namespace communication
{
class CertificateData;
class Communicator;
}

namespace client
{

class Context;

class AuthManager
{
public:
    AuthManager(std::weak_ptr<Context> context, std::string defaultHostname,
                const unsigned int port, const bool checkCertificate);

    std::pair<bool, std::string> authenticateWithCertificate(const bool debugGsi);
    bool authenticateWithToken(std::string globalRegistryUrl,
                               const unsigned int port);

    std::shared_ptr<communication::Communicator> createCommunicator(
            const unsigned int dataPoolSize,
            const unsigned int metaPoolSize) const;

private:
    std::weak_ptr<Context> m_context;
    std::string m_hostname;
    const unsigned int m_port;
    const bool m_checkCertificate;
    std::shared_ptr<communication::CertificateData> m_certificateData;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_AUTH_MANAGER_H
