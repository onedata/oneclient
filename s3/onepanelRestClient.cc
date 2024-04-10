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

    logRequest("Onepanel", request);

    auto &requestStream = session_.sendRequest(request);
    requestStream << bodyStr;

    Poco::Net::HTTPResponse response;
    auto responseStream = toString(session_.receiveResponse(response));

    logResponse("Onepanel", responseStream);

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

} // namespace onepanel
} // namespace rest
} // namespace one