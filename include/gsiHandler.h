/**
 * @file gsiHandler.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef GSI_HANDLER_H
#define GSI_HANDLER_H

#include <iostream>
#include "communicationHandler.h"

#define BASE_DOMAIN "cluster.veilfs.plgrid.pl"

namespace veil {
namespace client {
namespace gsi {

    extern bool debug;

    bool validateProxyConfig();
    bool validateProxyCert();
    std::string getClusterHostname();
    CertificateInfo getCertInfo();

} // namespace gsi
} // namespace client
} // namespace veil


#endif // GSI_HANDLER_H
