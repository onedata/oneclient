/**
 * @file clusterProxyHelper_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CLUSTER_PROXY_HELPER_PROXY_H
#define CLUSTER_PROXY_HELPER_PROXY_H


#include "clusterProxyHelper.h"

#include <memory>

class ProxyClusterProxyHelper: public one::helpers::ClusterProxyHelper {
public:
    ProxyClusterProxyHelper(std::shared_ptr<one::communication::Communicator> communicator,
                            const ArgsMap &args)
        : ClusterProxyHelper{std::move(communicator), one::helpers::BufferLimits{}, args}
    {
    }

    using one::helpers::ClusterProxyHelper::doWrite;
    using one::helpers::ClusterProxyHelper::doRead;
};


#endif // CLUSTER_PROXY_HELPER_PROXY_H
