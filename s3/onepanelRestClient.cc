/**
 * @file onepanelRestClient.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "onepanelRestClient.h"
#include "helpers/logging.h"

#include <Poco/Net/HTTPBasicCredentials.h>
#include <boost/algorithm/algorithm.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/variant/get.hpp>

namespace one {
namespace rest {
namespace onepanel {

OnepanelClient::OnepanelClient(const std::string &hostname)
    : credentials_{OnepanelNoAuth{}}
{
    session_.setHost(hostname);
    session_.setKeepAlive(true);
}

OnepanelClient::~OnepanelClient() { session_.reset(); }

void OnepanelClient::setCredentials(OnepanelCredentials credentials)
{
    credentials_ = std::move(credentials);
}

void OnepanelClient::supportSpace(
    const std::string &supportToken, const std::string &storageId, size_t size)
{
    std::string result;

    Poco::JSON::Object body;
    body.set("token", supportToken);
    body.set("storageId", storageId);
    body.set("size", size);

    auto bodyStr = toString(body);

    Poco::Net::HTTPRequest request{
        Poco::Net::HTTPRequest::HTTP_POST, "/api/v3/onepanel/provider/spaces"};
    request.setContentType("application/json");

    updateRequestCredentials(request);

    request.setContentLength(bodyStr.size());

    auto &requestStream = session_.sendRequest(request);
    requestStream << bodyStr;

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_CREATED) {
        throw Poco::Net::HTTPException(statusCode);
    }
}

void OnepanelClient::updateRequestCredentials(
    Poco::Net::HTTPRequest &request) const
{
    if (boost::get<OnepanelBasicAuth>(&credentials_) != nullptr) {
        std::vector<std::string> basicAuthPair;

        boost::algorithm::split(basicAuthPair,
            boost::get<OnepanelBasicAuth>(&credentials_)->credentials,
            boost::is_any_of(":"));

        if (basicAuthPair.size() != 2) {
            throw Poco::Net::HTTPException(Poco::Net::HTTPResponse::HTTPStatus::
                    HTTP_INTERNAL_SERVER_ERROR);
        }

        Poco::Net::HTTPBasicCredentials creds(
            basicAuthPair.at(0), basicAuthPair.at(1));

        creds.authenticate(request);
    }
    else if (boost::get<OnepanelTokenAuth>(&credentials_) != nullptr) {
        request.add("X-Auth-Token",
            boost::get<OnepanelTokenAuth>(&credentials_)->token);
    }
}

std::string OnepanelClient::getProviderId()
{
    Poco::Net::HTTPRequest request{
        Poco::Net::HTTPRequest::HTTP_GET, "/configuration"};

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throw Poco::Net::HTTPException(statusCode);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStream);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    return object->getValue<std::string>("providerId");
}

bool OnepanelClient::isSpaceSupported(
    const std::string &spaceId, const std::string &providerId)
{
    std::string result;

    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_GET,
        std::string("/api/v3/onepanel/provider/spaces/") + spaceId};
    request.setContentType("application/json");
    updateRequestCredentials(request);
    request.setContentLength(0);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    auto statusCode = response.getStatus();

    if (statusCode == Poco::Net::HTTPResponse::HTTP_FORBIDDEN ||
        statusCode == Poco::Net::HTTPResponse::HTTP_UNAUTHORIZED) {
        return false;
    }

    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throw Poco::Net::HTTPException(statusCode);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStream);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    return object->has("supportingProviders") &&
        object->getObject("supportingProviders")->has(providerId);
}

bool OnepanelClient::ensureSpaceIsSupported(const std::string &spaceId)
{
    using namespace std::literals::chrono_literals; // NOLINT

    const std::string providerId = getProviderId();

    constexpr auto kRetryCountMax = 100UL;

    auto retryCount = kRetryCountMax;
    while (retryCount-- != 0) {
        if (isSpaceSupported(spaceId, providerId))
            return true;

        std::this_thread::sleep_for(100ms);
    }

    return false;
}
} // namespace onepanel
} // namespace rest
} // namespace one