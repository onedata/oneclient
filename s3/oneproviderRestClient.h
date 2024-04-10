/**
 * @file oneproviderRestClient.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "restCommon.h"

#include <Poco/JSON/Object.h>
#include <Poco/JSON/Parser.h>
#include <Poco/Net/HTTPRequest.h>
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/HTTPSClientSession.h>
#include <Poco/Net/NetException.h>
#include <Poco/URI.h>

#include <boost/variant.hpp>
#include <chrono>
#include <thread>

namespace one {
namespace rest {
namespace oneprovider {

class OneproviderClient {
public:
    OneproviderClient(const std::string &hostname);

    ~OneproviderClient();

    std::string getProviderId();

    bool isSpaceSupported(const std::string &spaceId,
        const std::string &providerId, const std::string &token);

    bool ensureSpaceIsSupported(
        const std::string &spaceId, const std::string &token);

private:
    Poco::Net::HTTPSClientSession session_;
};
} // namespace oneprovider
} // namespace rest
} // namespace one