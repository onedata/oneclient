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

/**
 * The AuthManager class is responsible for setting an authentication scheme
 * for Client - Provider communication.
 */
class AuthManager
{
public:
    /**
     * Constructor.
     * @param context An application context.
     * @param defaultHostname A default hostname to be used for communication
     * with a Provider. The hostname ist used as a base for a generated hostname
     * in certificate-based authorization.
     * @param port A port to be used for communication with a Provider.
     * @param checkCertificate Determines whether to check Provider's and
     * Global Registry's server certificates for validity.
     */
    AuthManager(std::weak_ptr<Context> context, std::string defaultHostname,
                const unsigned int port, const bool checkCertificate);

    /**
     * Sets up the object for certificate-based authentication.
     * @param debugGsi Determines whether to enable more detailed (debug) logs.
     */
    void authenticateWithCertificate(const bool debugGsi);

    /**
     * Sets up the object for token-based authentication.
     * @param globalRegistryHostname A hostname of Global Registry to be used
     * for token-based authentication.
     * @param globalRegistryPort A port of GlobalRegistry to be used for
     * token-based authentication
     */
    void authenticateWithToken(std::string globalRegistryHostname,
                               const unsigned int globalRegistryPort);

    /**
     * Creates a @c veil::communication::Communicator object set up with proper
     * authentication settings.
     * @see veil::communication::createCommunicator
     * @param dataPoolSize The size of data pool to be created.
     * @param metaPoolSize The size of meta pool to be created.
     * @return A new instance of @c Communicator .
     */
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
