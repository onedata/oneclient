/**
 * @file onepanelRestClient.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "onepanelRestClient.h"

namespace one {
namespace rest {
namespace onepanel {

OnepanelClient::OnepanelClient(const std::string &hostname)
{
    session_.setHost(hostname);
    session_.setKeepAlive(true);
}

OnepanelClient::~OnepanelClient() { session_.reset(); }

void OnepanelClient::supportSpace(const std::string &token,
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
    request.add("X-Auth-Token", token);
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

bool OnepanelClient::isSpaceSupported(const std::string &token,
    const std::string &spaceId, const std::string &providerId)
{
    std::string result;

    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_GET,
        std::string("/api/v3/onepanel/provider/spaces/") + spaceId};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(0);

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

    return object->has("supportingProviders") &&
        object->getObject("supportingProviders")->has(providerId);
}

std::vector<std::pair<std::string, std::string>> OnepanelClient::listUserSpaces(
    const std::string &token)
{
    std::vector<std::pair<std::string, std::string>> result;

    Poco::Net::HTTPRequest request{
        Poco::Net::HTTPRequest::HTTP_GET, "/api/v3/oneprovider/spaces"};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(0);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throw Poco::Net::HTTPException(statusCode);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStream);
    Poco::JSON::Array::Ptr spaces = value.extract<Poco::JSON::Array::Ptr>();

    for (const auto &space : *spaces) {
        Poco::JSON::Object::Ptr spaceDetails =
            space.extract<Poco::JSON::Object::Ptr>();
        result.emplace_back(
            spaceDetails->get("name"), spaceDetails->get("spaceId"));
    }

    return result;
}

bool OnepanelClient::ensureSpaceIsSupported(
    const std::string &token, const std::string &spaceId)
{
    using namespace std::literals::chrono_literals; // NOLINT

    const std::string providerId = getProviderId();

    auto retry_count = 100;
    while (retry_count--) {
        if (isSpaceSupported(token, spaceId, providerId))
            return true;

        std::this_thread::sleep_for(100ms);
    }

    return false;
}
}
}
}