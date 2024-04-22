/**
 * @file restCommon.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "restCommon.h"

#include "logging.h"

namespace one {
namespace rest {

void throwHTTPExceptionFromRESTErrorResponse(const std::string &response)
{
    Poco::JSON::Parser p;
    auto value = p.parse(response);
    Poco::JSON::Object::Ptr object = value.extract<Poco::JSON::Object::Ptr>();

    if (VLOG_IS_ON(1)) {
        std::stringstream ss;
        object->stringify(ss);

        LOG_DBG(1) << "Response error: " << ss.str();
    }

    if (object.isNull())
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_INTERNAL_SERVER_ERROR);

    const auto errorObject = object->getObject("error");

    if (!errorObject->has("id"))
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_INTERNAL_SERVER_ERROR);

    const auto &errorId = errorObject->getValue<std::string>("id");

    if (errorId == "unauthorized")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "forbidden")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_FORBIDDEN);
    if (errorId == "badMessage")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_BAD_REQUEST);
    if (errorId == "notImplemented")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_NOT_IMPLEMENTED);
    if (errorId == "serviceUnavailable")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_SERVICE_UNAVAILABLE);
    if (errorId == "timeout")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_REQUEST_TIMEOUT);
    if (errorId == "notFound")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_NOT_FOUND);
    if (errorId == "alreadyExists")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_CONFLICT);
    if (errorId == "userBlocked")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_FORBIDDEN);
    if (errorId == "badBasicCredentials")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "badToken")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "badValueToken")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "badServiceToken")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "badConsumerToken")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "tokenInvalid")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "notAnAccessToken")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    if (errorId == "cannotRemoveLastOwner")
        throw Poco::Net::HTTPException(
            Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED);
    throw Poco::Net::HTTPException(
        Poco::Net::HTTPResponse::HTTPStatus::HTTP_INTERNAL_SERVER_ERROR);
}

std::string toString(const Poco::JSON::Object &o)
{
    std::ostringstream os;
    o.stringify(os);
    return os.str();
}

std::string toString(std::istream &is)
{
    std::ostringstream os;
    os << is.rdbuf();
    return os.str();
}

void logRequest(
    const std::string &service, const Poco::Net::HTTPRequest &request)
{
    if (VLOG_IS_ON(ONES3_REST_LOG_LEVEL)) {
        std::stringstream ss;
        request.write(ss);

        LOG_DBG(ONES3_REST_LOG_LEVEL) << service << " REST request:\n"
                                      << ss.str();
    }
}

void logResponse(const std::string &service, const std::string &response)
{
    LOG_DBG(ONES3_REST_LOG_LEVEL) << service << " REST response:\n" << response;
}

} // namespace rest
} // namespace one