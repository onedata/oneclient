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
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/NetException.h>

#include <string>

namespace one {
namespace rest {

void throwHTTPExceptionFromRESTErrorResponse(std::istream &responseStream);

std::string toString(const Poco::JSON::Object &o);

std::string toString(std::istream &is);

}
}