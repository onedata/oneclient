/**
 * @file httpMock.h
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <Poco/JSON/Parser.h>
#include <Poco/Net/Context.h>
#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPServer.h>
#include <Poco/Net/HTTPServerParams.h>
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/KeyConsoleHandler.h>
#include <Poco/Net/PrivateKeyPassphraseHandler.h>
#include <Poco/Net/SSLManager.h>
#include <Poco/Net/SecureServerSocket.h>
#include <Poco/Net/ServerSocket.h>
#include <Poco/Path.h>
#include <Poco/ThreadPool.h>
#include <Poco/URI.h>
#include <Poco/Util/HelpFormatter.h>
#include <Poco/Util/IniFileConfiguration.h>
#include <Poco/Util/LayeredConfiguration.h>
#include <Poco/Util/Option.h>
#include <Poco/Util/OptionSet.h>
#include <Poco/Util/PropertyFileConfiguration.h>
#include <Poco/Util/ServerApplication.h>

#include <atomic>
#include <iostream>
#include <map>

namespace httpmock {

using namespace Poco::Net;
using namespace Poco::Util;
using namespace Poco::JSON;
using namespace Poco::Dynamic;
using namespace std;

class ResponseHandler : public HTTPRequestHandler {
public:
    ResponseHandler(map<string, string> &responses);

    void handleRequest(
        HTTPServerRequest &request, HTTPServerResponse &response) override;

private:
    map<string, string> &_responses;
};

class SetResponseHandler : public HTTPRequestHandler {
public:
    SetResponseHandler(map<string, string> &responses)
        : _responses(responses)
    {
    }

    void handleRequest(
        HTTPServerRequest &request, HTTPServerResponse &response) override
    {
        Poco::URI uri(request.getURI());
        string path = uri.getPath();
        if (path.find("/set_response/") == 0) {
            path = path.substr(13);
            istream &istr = request.stream();
            string body;
            Poco::StreamCopier::copyToString(istr, body);

            _responses["/" + path] = body;

            response.setStatus(HTTPResponse::HTTP_OK);
            response.send() << "{\"status\":\"ok\"}";
        }
        else {
            response.setStatus(HTTPResponse::HTTP_BAD_REQUEST);
            response.send() << "{\"error\":\"Invalid endpoint\"}";
        }
    }

private:
    map<string, string> &_responses;
};

class RequestHandlerFactory : public HTTPRequestHandlerFactory {
public:
    RequestHandlerFactory(map<string, string> &responses);

    HTTPRequestHandler *createRequestHandler(
        const HTTPServerRequest &request) override;

private:
    map<string, string> &_responses;
};

class HTTPMock : public ServerApplication {
public:
    void set_response(const std::string &path, const std::string &body);

    void stop();

protected:
    int main(const vector<string> &args) override;

private:
    map<string, string> _responses;
    std::atomic_bool _stop{false};
};
}
