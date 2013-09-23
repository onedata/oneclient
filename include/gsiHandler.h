/**
 * @file gsiHandler.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef GSI_HANDLER_H
#define GSI_HANDLER_H

#include <iostream>

#define GSI_INIT_COMMAND string("grid-proxy-init")
#define GSI_INFO_COMMAND string("grid-proxy-info")

#define BASE_DOMAIN "cluster.veilfs.com"

namespace veil {
namespace client {
namespace gsi {

    extern bool debug;

    bool validateProxyConfig();
    bool validateProxyCert();
    std::string getProxyCertPath();
    std::string getClusterHostname();

} // namespace gsi
} // namespace client
} // namespace veil


#endif // GSI_HANDLER_H
