/**
 * @file onepanelRestClient.h
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

#include <chrono>
#include <thread>

namespace one {
namespace rest {
namespace onepanel {

namespace model {

}

class OnepanelClient {
public:
    OnepanelClient(const std::string &hostname);

    ~OnepanelClient();

    void supportSpace(const std::string &token, const std::string &supportToken,
        const std::string &storageId, size_t size);

    std::string getProviderId();

    bool isSpaceSupported(const std::string &token, const std::string &spaceId,
        const std::string &providerId);

    bool ensureSpaceIsSupported(
        const std::string &token, const std::string &spaceId);

    std::vector<std::pair<std::string, std::string>> listUserSpaces(const std::string &token);

private:
    Poco::Net::HTTPSClientSession session_;
};
}
}
}