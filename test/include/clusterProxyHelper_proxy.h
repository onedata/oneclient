/**
 * @file clusterProxyHelper_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CLUSTER_PROXY_HELPER_PROXY_H
#define CLUSTER_PROXY_HELPER_PROXY_H

#include "clusterProxyHelper.h"

using namespace veil;
using namespace veil::helpers;

using namespace std;
using namespace veil::protocol::remote_file_management;
using namespace veil::protocol::communication_protocol;

class ProxyClusterProxyHelper
    : public ClusterProxyHelper {
public:
    ProxyClusterProxyHelper(std::vector<std::string> args) : ClusterProxyHelper(args)
    {
    }

    protocol::communication_protocol::Answer sendCluserMessage(protocol::communication_protocol::ClusterMsg &msg) {
        return ClusterProxyHelper::sendCluserMessage(msg);
    }

    protocol::communication_protocol::ClusterMsg commonClusterMsgSetup(std::string inputType, std::string inputData) {
        return ClusterProxyHelper::commonClusterMsgSetup(inputType, inputData);
    }

    std::string requestMessage(std::string inputType, std::string answerType, std::string inputData) {
        return ClusterProxyHelper::requestMessage(inputType, answerType, inputData);
    } 

    std::string requestAtom(std::string inputType, std::string inputData) {
        return ClusterProxyHelper::requestAtom(inputType, inputData);
    }

};

#endif // CLUSTER_PROXY_HELPER_PROXY_H