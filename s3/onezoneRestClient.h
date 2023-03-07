/**
 * @file onezoneRestClient.h
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

namespace one {
namespace rest {
namespace onezone {

namespace model {

struct UserSpaceDetails {
    std::string spaceId;
    std::string name;
    std::map<std::string, size_t> providers;
    unsigned long long creationTime;
};

}

class OnezoneClient {
public:
    OnezoneClient(const std::string &hostname);

    ~OnezoneClient();

    std::string createSpaceSupportToken(
        const std::string &token, const std::string &spaceId);

    std::vector<std::string> listUserSpaces(const std::string &token);

    model::UserSpaceDetails getUserSpace(
        const std::string &token, const std::string &spaceId);

    std::string createSpace(const std::string &token, const std::string &name);

    void deleteSpace(const std::string &token, const std::string &spaceId);
private:
    Poco::Net::HTTPSClientSession session_;
};
}
}
}