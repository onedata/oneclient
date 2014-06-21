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

#include "context.h"

#include <memory>

struct ProxyConfig: public veil::client::Config {
    ProxyConfig(std::shared_ptr<veil::client::Context> context)
        : veil::client::Config{std::move(context)} {}

    std::string getFuseID()
    {
        return fuseID;
    }

    std::string fuseID;
};

#endif // CONFIG_PROXY_H
