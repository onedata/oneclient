/**
 * @file oneproviderRestClient.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "oneproviderRestClient.h"

#include "helpers/logging.h"

#include <Poco/Net/HTTPBasicCredentials.h>
#include <boost/algorithm/string/classification.hpp>

namespace one {
namespace rest {
namespace oneprovider {

OneproviderClient::OneproviderClient(const std::string &hostname)
{
    session_.setHost(hostname);
    session_.setKeepAlive(true);
}

OneproviderClient::~OneproviderClient() { session_.reset(); }

bool OneproviderClient::isSpaceSupported(const std::string &spaceId,
    const std::string &providerId, const std::string &token)
{
    std::string result;

    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_GET,
        std::string("/api/v3/oneprovider/data/") + spaceId};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(0);

    logRequest("Oneprovider", request);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    logResponse("Oneprovider", responseStream);

    auto statusCode = response.getStatus();

    if (statusCode == Poco::Net::HTTPResponse::HTTP_FORBIDDEN ||
        statusCode == Poco::Net::HTTPResponse::HTTP_UNAUTHORIZED) {
        return false;
    }

    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throw Poco::Net::HTTPException(statusCode);
    }

    return true;
}

std::string OneproviderClient::getProviderId()
{
    Poco::Net::HTTPRequest request{
        Poco::Net::HTTPRequest::HTTP_GET, "/api/v3/onepanel/configuration"};

    logRequest("Oneprovider", request);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    logResponse("Oneprovider", responseStream);

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throw Poco::Net::HTTPException(statusCode);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStream);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    return object->getValue<std::string>("providerId");
}

bool OneproviderClient::ensureSpaceIsSupported(
    const std::string &spaceId, const std::string &token)
{
    using namespace std::literals::chrono_literals; // NOLINT

    const std::string providerId = getProviderId();

    constexpr auto kRetryCountMax = 100UL;
    const auto kRetryDelay = 100ms;

    auto retryCount = kRetryCountMax;
    while (retryCount-- != 0) {
        if (isSpaceSupported(spaceId, providerId, token))
            return true;

        std::this_thread::sleep_for(kRetryDelay);
    }

    return false;
}
} // namespace oneprovider
} // namespace rest
} // namespace one