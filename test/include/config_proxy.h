/**
 * @file config_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CONFIG_PROXY_H
#define CONFIG_PROXY_H

#include "config.h"
#include "testCommon.h"

class ProxyConfig
    : public veil::client::Config {
public:
    void setFuseID(const std::string &fuseID)
    {
        m_globalNode[FUSE_ID_OPT] = fuseID;
    }

public:
};



#endif // CONFIG_PROXY_H
