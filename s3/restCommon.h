/**
 * @file restCommon.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <Poco/JSON/Object.h>
#include <Poco/JSON/Parser.h>
#include <Poco/Net/HTTPRequest.h>
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/NetException.h>

#include <string>

namespace one {
namespace rest {

#define ONES3_REST_LOG_LEVEL 4

void throwHTTPExceptionFromRESTErrorResponse(const std::string &response);

std::string toString(const Poco::JSON::Object &o);

std::string toString(std::istream &is);

void logRequest(
    const std::string &service, const Poco::Net::HTTPRequest &request);

void logRequest(const std::string &service,
    const Poco::Net::HTTPRequest &request, const Poco::JSON::Object &body);

void logResponse(const std::string &service, const std::string &response);

} // namespace rest
} // namespace one