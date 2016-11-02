/**
 * @file swiftHelper.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "swiftHelper.h"
#include "logging.h"

#include <glog/stl_logging.h>

#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>

namespace std {
template <> struct hash<Poco::Net::HTTPResponse::HTTPStatus> {
    size_t operator()(const Poco::Net::HTTPResponse::HTTPStatus &p) const
    {
        return std::hash<int>()(static_cast<int>(p));
    }
};
}

namespace one {
namespace helpers {

namespace {

std::unordered_map<Poco::Net::HTTPResponse::HTTPStatus, std::errc> errors = {
    {Poco::Net::HTTPResponse::HTTPStatus::HTTP_NOT_FOUND,
        std::errc::no_such_file_or_directory},
    {Poco::Net::HTTPResponse::HTTPStatus::HTTP_REQUESTED_RANGE_NOT_SATISFIABLE,
        std::errc::no_such_file_or_directory},
    {Poco::Net::HTTPResponse::HTTPStatus::HTTP_REQUEST_TIMEOUT,
        std::errc::timed_out},
    {Poco::Net::HTTPResponse::HTTPStatus::HTTP_LENGTH_REQUIRED,
        std::errc::invalid_argument},
    {Poco::Net::HTTPResponse::HTTPStatus::HTTP_UNAUTHORIZED,
        std::errc::permission_denied},
};

template <typename Outcome> error_t getReturnCode(const Outcome &outcome)
{
    auto statusCode = outcome->getResponse()->getStatus();

    auto error = std::errc::io_error;
    auto search = errors.find(statusCode);
    if (search != errors.end())
        error = search->second;

    return std::error_code(static_cast<int>(error), std::system_category());
}

template <typename Outcome>
void throwOnError(std::string operation, const Outcome &outcome)
{
    if (outcome->getError().code == Swift::SwiftError::SWIFT_OK)
        return;

    auto code = getReturnCode(outcome);
    auto reason = "'" + operation + "': " + outcome->getError().msg;

    throw std::system_error{code, reason};
}
}

SwiftHelper::SwiftHelper(std::unordered_map<std::string, std::string> args)
    : m_args{std::move(args)}
{
}

CTXPtr SwiftHelper::createCTX(
    std::unordered_map<std::string, std::string> params)
{
    return std::make_shared<SwiftHelperCTX>(std::move(params), m_args);
}

asio::mutable_buffer SwiftHelper::getObject(
    CTXPtr rawCTX, std::string key, asio::mutable_buffer buf, off_t offset)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto &account = ctx->authenticate();
    auto size = asio::buffer_size(buf);

    Swift::Container container(account.get(), ctx->getContainerName());
    Swift::Object object(&container, key);

    auto headers = std::vector<Swift::HTTPHeader>({Swift::HTTPHeader("Range",
        rangeToString(offset, static_cast<off_t>(offset + size - 1)))});
    auto getResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
        object.swiftGetObjectContent(nullptr, &headers));
    throwOnError("getObject", getResponse);

    std::string objectData{
        std::istreambuf_iterator<char>(*getResponse->getPayload()), {}};

    auto copied = asio::buffer_copy(buf, asio::buffer(objectData));
    return asio::buffer(buf, copied);
}

off_t SwiftHelper::getObjectsSize(
    CTXPtr rawCTX, const std::string &prefix, std::size_t objectSize)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto &account = ctx->authenticate();

    Swift::Container container(account.get(), ctx->getContainerName());
    auto params = std::vector<Swift::HTTPHeader>(
        {Swift::HTTPHeader("prefix", adjustPrefix(prefix))});

    auto listResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
        container.swiftListObjects(
            Swift::HEADER_FORMAT_APPLICATION_JSON, &params, true));
    throwOnError("getObjectsSize", listResponse);

    boost::property_tree::ptree pt;
    boost::property_tree::read_json(*listResponse->getPayload(), pt);
    if (pt.size() == 0) {
        return 0;
    }

    auto key = pt.get<std::string>(".name");
    auto size = pt.get<uint64_t>(".bytes");

    return getObjectId(std::move(key)) * objectSize + size;
}

std::size_t SwiftHelper::putObject(
    CTXPtr rawCTX, std::string key, asio::const_buffer buf)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto &account = ctx->authenticate();
    auto size = asio::buffer_size(buf);
    auto data = asio::buffer_cast<const char *>(buf);

    Swift::Container container(account.get(), ctx->getContainerName());
    Swift::Object object(&container, key);

    auto createResponse = std::unique_ptr<Swift::SwiftResult<int *>>(
        object.swiftCreateReplaceObject(data, size, true));
    throwOnError("putObject", createResponse);

    return asio::buffer_size(buf);
}

void SwiftHelper::deleteObjects(CTXPtr rawCTX, std::vector<std::string> keys)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto &account = ctx->authenticate();

    Swift::Container container(account.get(), ctx->getContainerName());
    for (uint i = 0; i < keys.size(); i += MAX_DELETE_OBJECTS) {
        auto deleteResponse =
            std::unique_ptr<Swift::SwiftResult<std::istream *>>(
                container.swiftDeleteObjects(
                    std::vector<std::string>(keys.begin() + i,
                        keys.begin() + std::min<size_t>(i + MAX_DELETE_OBJECTS,
                                           keys.size()))));

        throwOnError("deleteObjects", deleteResponse);
    }
}

std::vector<std::string> SwiftHelper::listObjects(
    CTXPtr rawCTX, std::string prefix)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto &account = ctx->authenticate();

    Swift::Container container(account.get(), ctx->getContainerName());
    auto params = std::vector<Swift::HTTPHeader>(
        {Swift::HTTPHeader("prefix", adjustPrefix(prefix)),
            Swift::HTTPHeader("limit", std::to_string(MAX_LIST_OBJECTS))});

    std::vector<std::string> objectsList{};
    while (true) {
        if (!objectsList.empty()) {
            params.pop_back();
            params.push_back(Swift::HTTPHeader("marker", objectsList.back()));
        }

        auto listResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
            container.swiftListObjects(
                Swift::HEADER_FORMAT_TEXT_XML, &params, true));
        throwOnError("listObjects", listResponse);

        auto lines = 0;
        for (std::string name;
             std::getline(*listResponse->getPayload(), name);) {
            ++lines;
            objectsList.push_back(name);
        }

        if (lines != MAX_LIST_OBJECTS)
            break;
    };

    return objectsList;
}

std::shared_ptr<SwiftHelperCTX> SwiftHelper::getCTX(CTXPtr rawCTX) const
{
    auto ctx = std::dynamic_pointer_cast<SwiftHelperCTX>(rawCTX);
    if (ctx == nullptr) {
        LOG(INFO) << "Helper changed. Creating new context with arguments: "
                  << m_args;
        return std::make_shared<SwiftHelperCTX>(rawCTX->parameters(), m_args);
    }
    return ctx;
}

SwiftHelperCTX::SwiftHelperCTX(
    std::unordered_map<std::string, std::string> params,
    std::unordered_map<std::string, std::string> args)
    : IStorageHelperCTX{std::move(params)}
    , m_args{std::move(args)}
{
}

void SwiftHelperCTX::setUserCTX(
    std::unordered_map<std::string, std::string> args)
{
    m_args.swap(args);
    m_args.insert(args.begin(), args.end());
}

std::unordered_map<std::string, std::string> SwiftHelperCTX::getUserCTX()
{
    return {{SWIFT_HELPER_USER_NAME_ARG, m_args.at(SWIFT_HELPER_USER_NAME_ARG)},
        {SWIFT_HELPER_PASSWORD_ARG, m_args.at(SWIFT_HELPER_PASSWORD_ARG)}};
}

const std::unique_ptr<Swift::Account> &SwiftHelperCTX::authenticate()
{
    std::lock_guard<std::mutex> guard{m_mutex};

    if (m_account)
        return m_account;

    Swift::AuthenticationInfo info;
    info.username = m_args.at(SWIFT_HELPER_USER_NAME_ARG);
    info.password = m_args.at(SWIFT_HELPER_PASSWORD_ARG);
    info.authUrl = m_args.at(SWIFT_HELPER_AUTH_URL_ARG);
    info.tenantName = m_args.at(SWIFT_HELPER_TENANT_NAME_ARG);
    info.method = Swift::AuthenticationMethod::KEYSTONE;

    auto authResponse = std::unique_ptr<Swift::SwiftResult<Swift::Account *>>(
        Swift::Account::authenticate(info, true));
    throwOnError("authenticate", authResponse);

    m_account = std::unique_ptr<Swift::Account>(authResponse->getPayload());
    authResponse->setPayload(nullptr);

    return m_account;
}

const std::string &SwiftHelperCTX::getContainerName() const
{
    return m_args.at(SWIFT_HELPER_CONTAINER_NAME_ARG);
}

} // namespace helpers
} // namespace one
