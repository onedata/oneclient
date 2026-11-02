/**
 * @file httpMock.cc
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "httpMock.h"

#include "helpers/logging.h"

#include <thread>

namespace httpmock {
ResponseHandler::ResponseHandler(map<string, string> &responses)
    : _responses(responses)
{
}

void ResponseHandler::handleRequest(
    HTTPServerRequest &request, HTTPServerResponse &response)
{
    response.setContentType("application/json");

    Poco::URI uri(request.getURI());
    string path = uri.getPath();
    auto it = _responses.find(path);
    if (it != _responses.end()) {
        response.setStatus(HTTPResponse::HTTP_OK);
        response.send() << it->second;
    }
    else {
        response.setStatus(HTTPResponse::HTTP_NOT_FOUND);
        response.send() << "{\"error\":\"Not found\"}";
    }
}

RequestHandlerFactory::RequestHandlerFactory(map<string, string> &responses)
    : _responses(responses)
{
}

HTTPRequestHandler *RequestHandlerFactory::createRequestHandler(
    const HTTPServerRequest &request)
{
    Poco::URI uri(request.getURI());
    string path = uri.getPath();
    if (path.find("/set_response/") == 0) {
        return new SetResponseHandler(_responses);
    }
    else {
        return new ResponseHandler(_responses);
    }
}

void HTTPMock::set_response(const std::string &path, const std::string &body)
{
    _responses[path] = body;
}

int HTTPMock::main(const vector<string> &args)
{
    std::string host = args[0];
    Poco::UInt16 port = std::atoi(args[1].c_str());
    int maxQueued = 100;
    int maxThreads = 16;

    // HTTP server parameters
    HTTPServerParams *pParams = new HTTPServerParams;
    pParams->setMaxQueued(maxQueued);
    pParams->setMaxThreads(maxThreads);

    // Create and start the HTTP server
    ServerSocket svs{SocketAddress{host, port}, 64};

    HTTPServer s(new RequestHandlerFactory(_responses), svs, pParams);

    s.start();

    while (!_stop) {
        using namespace chrono_literals;
        std::this_thread::sleep_for(1s);
    }

    // Stop the HTTP server
    s.stop();

    return Application::EXIT_OK;
}

void HTTPMock::stop() { _stop = true; }
}