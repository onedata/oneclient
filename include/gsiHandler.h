/**
 * @file gsiHandler.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_GSI_HANDLER_H
#define VEILCLIENT_GSI_HANDLER_H


#include "communication/certificateData.h"

#include <memory>
#include <string>

namespace boost { namespace filesystem { class path; } }

namespace veil
{

static constexpr const char *BASE_DOMAIN = "cluster.veilfs.plgrid.pl";

inline std::string CONFIRM_CERTIFICATE_PROMPT(const std::string &USERNAME)
{
    return "Warning ! You are trying to connect using unconfirmed certificate "
           "as: '" + USERNAME + "'. Is it your account? (y/n): ";
}

namespace client
{

class Context;

class GSIHandler
{
public:
    GSIHandler(std::shared_ptr<Context> context, const bool debug = false);

    bool validateProxyConfig();
    bool validateProxyCert();
    std::string getClusterHostname();
    std::shared_ptr<communication::CertificateData> getCertData();

private:
    std::pair<std::string, std::string> findUserCertAndKey(const boost::filesystem::path &dir);
    std::pair<std::string, std::string> findUserCertAndKey();
    std::pair<std::string, std::string> getUserCertAndKey();
    const std::vector<std::pair<std::string, std::string>> &getCertSearchPath();

    const std::shared_ptr<Context> m_context;
    const bool m_debug;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_GSI_HANDLER_H
