/**
 * @file onezoneRestClient.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "onezoneRestClient.h"

#include "logging.h"

namespace one {
namespace rest {
namespace onezone {

OnezoneClient::OnezoneClient(const std::string &hostname)
{
    session_.setHost(hostname);
    session_.setKeepAlive(true);
}

OnezoneClient::~OnezoneClient() { session_.reset(); }

std::string OnezoneClient::createSpaceSupportToken(
    const std::string &token, const std::string &spaceId)
{
    constexpr auto kCaveatValidTime = 36000UL;

    std::string result;

    Poco::JSON::Object inviteToken;
    inviteToken.set("inviteType", "supportSpace");
    inviteToken.set("spaceId", spaceId);

    Poco::JSON::Object type;
    type.set("inviteToken", inviteToken);

    Poco::JSON::Object caveat;
    caveat.set("type", "time");
    caveat.set("validUntil", std::time(nullptr) + kCaveatValidTime);

    Poco::JSON::Array caveats;
    caveats.add(caveat);

    Poco::JSON::Object body;
    body.set("type", type);
    body.set("caveats", caveats);

    auto bodyStr = toString(body);

    LOG_DBG(3) << "Creating space support token: " << bodyStr;

    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_POST,
        "/api/v3/onezone/user/tokens/temporary"};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(bodyStr.size());

    logRequest("Onezone", request);

    auto &requestStream = session_.sendRequest(request);
    requestStream << bodyStr;

    Poco::Net::HTTPResponse response;
    auto responseStr = toString(session_.receiveResponse(response));

    logResponse("Onezone", responseStr);

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_CREATED) {
        throwHTTPExceptionFromRESTErrorResponse(responseStr);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStr);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    return object->getValue<std::string>("token");
}

std::vector<model::Space> OnezoneClient::listUserSpaces(
    const std::string &token)
{
    std::vector<model::Space> result;

    Poco::JSON::Object body;
    body.set("token", token);
    auto bodyStr = toString(body);

    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_POST,
        "/api/v3/onezone/tokens/infer_access_token_scope"};
    request.setContentType("application/json");
    request.setContentLength(bodyStr.size());

    logRequest("Onezone", request, body);

    auto &requestStream = session_.sendRequest(request);
    requestStream << bodyStr;

    Poco::Net::HTTPResponse response;

    auto responseStr = toString(session_.receiveResponse(response));

    logResponse("Onezone", responseStr);

    auto statusCode = response.getStatus();

    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throwHTTPExceptionFromRESTErrorResponse(responseStr);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStr);

    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    for (const auto &space :
        *object->getObject("dataAccessScope")->getObject("spaces")) {
        model::Space s;
        s.id = space.first;
        s.name = space.second.extract<Poco::JSON::Object::Ptr>()
                     ->get("name")
                     .toString();

        result.emplace_back(std::move(s));
    }

    return result;
}

model::UserSpaceDetails OnezoneClient::getUserSpace(
    const std::string &token, const std::string &spaceId)
{
    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_GET,
        "/api/v3/onezone/user/spaces/" + spaceId};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(0);

    logRequest("Onezone", request);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStr = toString(session_.receiveResponse(response));

    logResponse("Onezone", responseStr);

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_OK) {
        throwHTTPExceptionFromRESTErrorResponse(responseStr);
    }

    Poco::JSON::Parser p;
    auto value = p.parse(responseStr);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    model::UserSpaceDetails result;
    result.spaceId = object->getValue<std::string>("spaceId");
    result.name = object->getValue<std::string>("name");
    result.creationTime = object->getValue<uint64_t>("creationTime");

    return result;
}

std::string OnezoneClient::createSpace(
    const std::string &token, const std::string &name)
{
    Poco::JSON::Object body;
    body.set("name", name);
    body.set("idGeneratorSeed", name);
    auto bodyStr = toString(body);

    Poco::Net::HTTPRequest request{
        Poco::Net::HTTPRequest::HTTP_POST, "/api/v3/onezone/user/spaces"};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(bodyStr.size());

    logRequest("Onezone", request);

    auto &requestStream = session_.sendRequest(request);
    requestStream << bodyStr;

    Poco::Net::HTTPResponse response;
    auto responseStr = toString(session_.receiveResponse(response));

    logResponse("Onezone", responseStr);

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_CREATED) {
        throwHTTPExceptionFromRESTErrorResponse(responseStr);
    }

    if (!response.has("Location")) {
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTP_INTERNAL_SERVER_ERROR);
    }

    Poco::URI locationPath{response.get("Location")};
    std::vector<std::string> locationPathSegments;
    locationPath.getPathSegments(locationPathSegments);

    return locationPathSegments.back();
}

void OnezoneClient::deleteSpace(
    const std::string &token, const std::string &spaceId)
{
    Poco::Net::HTTPRequest request{Poco::Net::HTTPRequest::HTTP_DELETE,
        std::string("/api/v3/onezone/user/spaces/") + spaceId};
    request.setContentType("application/json");
    request.add("X-Auth-Token", token);
    request.setContentLength(0);

    logRequest("Onezone", request);

    session_.sendRequest(request);

    Poco::Net::HTTPResponse response;
    auto responseStr = toString(session_.receiveResponse(response));

    logResponse("Onezone", responseStr);

    auto statusCode = response.getStatus();
    if (statusCode != Poco::Net::HTTPResponse::HTTP_NO_CONTENT) {
        throwHTTPExceptionFromRESTErrorResponse(responseStr);
    }
}

} // namespace onezone
} // namespace rest
} // namespace one
