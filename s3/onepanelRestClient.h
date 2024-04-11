/**
 * @file onepanelRestClient.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "restCommon.h"

#include "options/options.h"

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
namespace onepanel {

struct OnepanelNoAuth { };

struct OnepanelTokenAuth {
    OnepanelTokenAuth(std::string t)
        : token{std::move(t)}
    {
    }

    std::string token;
};

struct OnepanelBasicAuth {
    OnepanelBasicAuth(std::string c)
        : credentials{std::move(c)}
    {
    }

    std::string credentials;
};

using OnepanelCredentials =
    boost::variant<OnepanelNoAuth, OnepanelTokenAuth, OnepanelBasicAuth>;

class OnepanelClient {
public:
    OnepanelClient(const std::string &hostname);

    ~OnepanelClient();

    void setCredentials(OnepanelCredentials credentials);

    void supportSpace(const std::string &supportToken,
        const std::string &storageId, size_t size);

private:
    void updateRequestCredentials(Poco::Net::HTTPRequest &request) const;

    Poco::Net::HTTPSClientSession session_;

    OnepanelCredentials credentials_;
};
} // namespace onepanel
} // namespace rest
} // namespace one