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

struct Provider {
    std::string providerId;
    std::string version;
    std::string name;
    std::string host;
    unsigned int port{443};
};

struct UserSpaceDetails {
    std::string spaceId;
    std::string name;
    std::map</* providerId */ std::string, size_t> providers;
    unsigned long long creationTime;
};

struct DataAccessScope {
    bool readonly;
    std::map<std::string, UserSpaceDetails> spaces;
    std::map<std::string, Provider> providers;

    void normalizeBucketNames(DataAccessScope &accessScope);
};

struct Space {
    std::string id;
    std::string name;
};
} // namespace model

class OnezoneClient {
public:
    OnezoneClient(const std::string &hostname, const uint16_t port = 443,
        const bool useTLS = true);

    ~OnezoneClient();

    std::string createSpaceSupportToken(
        const std::string &token, const std::string &spaceId);

    std::vector<model::Space> listUserSpaces(const std::string &token);

    std::vector<model::UserSpaceDetails> listUserSpacesDetails(
        const std::string &token);

    model::DataAccessScope inferAccessTokenScope(const std::string &token);

    std::map<std::string, model::Provider> getUserProviders(
        const std::string &token);

    model::UserSpaceDetails getUserSpace(
        const std::string &token, const std::string &spaceId);

    std::string createSpace(const std::string &token, const std::string &name);

    void deleteSpace(const std::string &token, const std::string &spaceId);

private:
    std::unique_ptr<Poco::Net::HTTPClientSession> session_;
};
} // namespace onezone
} // namespace rest
} // namespace one