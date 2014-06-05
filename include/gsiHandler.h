/**
 * @file gsiHandler.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef GSI_HANDLER_H
#define GSI_HANDLER_H

#include "communicationHandler.h"

#include <iostream>
#include <memory>
#include <string>

#define BASE_DOMAIN "cluster.veilfs.plgrid.pl"

namespace veil {
namespace client {

class Context;

class GSIHandler
{
public:
    GSIHandler(std::shared_ptr<Context> context, const bool debug = false);

    bool validateProxyConfig();
    bool validateProxyCert();
    std::string getClusterHostname();
    CertificateInfo getCertInfo();

private:
    std::pair<std::string, std::string> getUserCertAndKey();

    std::shared_ptr<Context> m_context;
    const bool m_debug;
};

} // namespace client
} // namespace veil


#endif // GSI_HANDLER_H
