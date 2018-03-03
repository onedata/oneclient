/**
 * @file swiftHelper.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "swiftHelper.h"
#include "logging.h"
#include "monitoring/monitoring.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Range.h>
#include <glog/stl_logging.h>

#if defined(__APPLE__)
#undef BOOST_BIND_NO_PLACEHOLDERS
#endif

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

template <typename Outcome>
std::error_code getReturnCode(const Outcome &outcome)
{
    LOG_FCALL() << LOG_FARG(outcome->getResponse()->getStatus());

    auto statusCode = outcome->getResponse()->getStatus();

    auto error = std::errc::io_error;
    auto search = errors.find(statusCode);
    if (search != errors.end())
        error = search->second;

    return std::error_code(static_cast<int>(error), std::system_category());
}

template <typename Outcome>
void throwOnError(folly::fbstring operation, const Outcome &outcome)
{
    LOG_FCALL() << LOG_FARG(operation)
                << LOG_FARG(outcome->getResponse()->getStatus());

    if (outcome->getError().code == Swift::SwiftError::SWIFT_OK)
        return;

    auto code = getReturnCode(outcome);
    auto reason =
        "'" + operation.toStdString() + "': " + outcome->getError().msg;

    LOG(ERROR) << "Operation " << operation << " failed with message "
               << outcome->getError().msg;

    throw std::system_error{code, std::move(reason)};
}
}

SwiftHelper::SwiftHelper(folly::fbstring containerName,
    const folly::fbstring &authUrl, const folly::fbstring &tenantName,
    const folly::fbstring &userName, const folly::fbstring &password,
    Timeout timeout)
    : m_auth{authUrl, tenantName, userName, password}
    , m_containerName{std::move(containerName)}
    , m_timeout{std::move(timeout)}
{
    LOG_FCALL() << LOG_FARG(containerName) << LOG_FARG(authUrl)
                << LOG_FARG(tenantName) << LOG_FARG(userName)
                << LOG_FARG(password);
}

folly::IOBufQueue SwiftHelper::getObject(
    const folly::fbstring &key, const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(key) << LOG_FARG(offset) << LOG_FARG(size);

    auto &account = m_auth.getAccount();

    Swift::Container container(&account, m_containerName.toStdString());
    Swift::Object object(&container, key.toStdString());

    folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};

    LOG_DBG(1) << "Attempting to read " << size << " bytes from object " << key
               << " at offset " << offset;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.swift.read");

    auto headers = std::vector<Swift::HTTPHeader>({Swift::HTTPHeader("Range",
        rangeToString(offset, static_cast<off_t>(offset + size - 1)))});
    auto getResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
        object.swiftGetObjectContent(nullptr, &headers));
    throwOnError("getObject", getResponse);

    char *data = static_cast<char *>(buf.preallocate(size, size).first);

    const auto newTail =
        std::copy(std::istreambuf_iterator<char>{*getResponse->getPayload()},
            std::istreambuf_iterator<char>{}, data);

    buf.postallocate(newTail - data);

    ONE_METRIC_TIMERCTX_STOP(
        timer, getResponse->getResponse()->getContentLength());

    LOG_DBG(1) << "Read " << size << " bytes from object " << key;

    return buf;
}

off_t SwiftHelper::getObjectsSize(
    const folly::fbstring &prefix, const std::size_t objectSize)
{
    LOG_FCALL() << LOG_FARG(prefix) << LOG_FARG(objectSize);

    auto &account = m_auth.getAccount();

    LOG_DBG(1) << "Attempting to get object " << prefix << " size";

    Swift::Container container(&account, m_containerName.toStdString());
    std::vector<Swift::HTTPHeader> params{
        {Swift::HTTPHeader("prefix", adjustPrefix(prefix))}};

    auto listResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
        container.swiftListObjects(
            Swift::HEADER_FORMAT_APPLICATION_JSON, &params, true));
    throwOnError("getObjectsSize", listResponse);

    boost::property_tree::ptree pt;
    boost::property_tree::read_json(*listResponse->getPayload(), pt);
    if (pt.size() == 0) {
        return 0;
    }

    auto key = pt.get<folly::fbstring>(".name");
    auto size = pt.get<uint64_t>(".bytes");

    return getObjectId(std::move(key)) * objectSize + size;
}

std::size_t SwiftHelper::putObject(
    const folly::fbstring &key, folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(key) << LOG_FARG(buf.chainLength());

    std::size_t writtenBytes = 0;
    auto &account = m_auth.getAccount();

    Swift::Container container(&account, m_containerName.toStdString());
    Swift::Object object(&container, key.toStdString());

    auto iobuf = buf.empty() ? folly::IOBuf::create(0) : buf.move();

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.swift.write");

    if (iobuf->isChained()) {
        iobuf->unshare();
        iobuf->coalesce();
    }

    LOG_DBG(1) << "Attempting to write object " << key << " of size "
               << iobuf->length();

    auto createResponse = std::unique_ptr<Swift::SwiftResult<int *>>(
        object.swiftCreateReplaceObject(
            reinterpret_cast<const char *>(iobuf->data()), iobuf->length(),
            true));

    throwOnError("putObject", createResponse);

    writtenBytes = iobuf->length();

    ONE_METRIC_TIMERCTX_STOP(timer, writtenBytes);

    LOG_DBG(1) << "Written " << writtenBytes << " bytes to object " << key;

    return writtenBytes;
}

void SwiftHelper::deleteObjects(const folly::fbvector<folly::fbstring> &keys)
{
    LOG_FCALL() << LOG_FARGV(keys);

    auto &account = m_auth.getAccount();

    LOG_DBG(1) << "Attempting to delete objects: " << LOG_VEC(keys);

    Swift::Container container(&account, m_containerName.toStdString());
    for (auto offset = 0u; offset < keys.size(); offset += MAX_DELETE_OBJECTS) {
        std::vector<std::string> keyBatch;

        const std::size_t batchSize =
            std::min<std::size_t>(keys.size() - offset, MAX_DELETE_OBJECTS);

        for (auto &key : folly::range(keys.begin(), keys.begin() + batchSize))
            keyBatch.emplace_back(key.toStdString());

        auto deleteResponse =
            std::unique_ptr<Swift::SwiftResult<std::istream *>>{
                container.swiftDeleteObjects(std::move(keyBatch))};

        throwOnError("deleteObjects", deleteResponse);
    }

    LOG_DBG(1) << "Deleted objects: " << LOG_VEC(keys);
}

folly::fbvector<folly::fbstring> SwiftHelper::listObjects(
    const folly::fbstring &prefix)
{
    LOG_FCALL() << LOG_FARG(prefix);

    auto &account = m_auth.getAccount();

    Swift::Container container(&account, m_containerName.toStdString());
    auto params = std::vector<Swift::HTTPHeader>(
        {Swift::HTTPHeader("prefix", adjustPrefix(prefix)),
            Swift::HTTPHeader("limit", std::to_string(MAX_LIST_OBJECTS))});

    LOG_DBG(1) << "Attempting to list objects at prefix " << prefix;

    folly::fbvector<folly::fbstring> objectsList;
    while (true) {
        if (!objectsList.empty()) {
            params.pop_back();
            params.push_back(
                Swift::HTTPHeader("marker", objectsList.back().toStdString()));
        }

        auto listResponse = std::unique_ptr<Swift::SwiftResult<std::istream *>>(
            container.swiftListObjects(
                Swift::HEADER_FORMAT_TEXT_XML, &params, true));

        throwOnError("listObjects", listResponse);

        auto lines = 0;
        for (std::string name;
             std::getline(*listResponse->getPayload(), name);) {
            ++lines;
            objectsList.emplace_back(std::move(name));
        }

        if (lines != MAX_LIST_OBJECTS)
            break;
    };

    LOG_DBG(1) << "Got object list at prefix " << prefix << ": "
               << LOG_VEC(objectsList);

    return objectsList;
}

SwiftHelper::Authentication::Authentication(const folly::fbstring &authUrl,
    const folly::fbstring &tenantName, const folly::fbstring &userName,
    const folly::fbstring &password)
{
    LOG_FCALL() << LOG_FARG(authUrl) << LOG_FARG(tenantName)
                << LOG_FARG(userName) << LOG_FARG(password);

    m_authInfo.username = userName.toStdString();
    m_authInfo.password = password.toStdString();
    m_authInfo.authUrl = authUrl.toStdString();
    m_authInfo.tenantName = tenantName.toStdString();
    m_authInfo.method = Swift::AuthenticationMethod::KEYSTONE;
}

Swift::Account &SwiftHelper::Authentication::getAccount()
{
    LOG_FCALL();

    std::lock_guard<std::mutex> guard{m_authMutex};
    if (m_account)
        return *m_account;

    auto authResponse = std::unique_ptr<Swift::SwiftResult<Swift::Account *>>(
        Swift::Account::authenticate(m_authInfo, true));
    throwOnError("authenticate", authResponse);

    m_account = std::unique_ptr<Swift::Account>(authResponse->getPayload());
    authResponse->setPayload(nullptr);

    return *m_account;
}

} // namespace helpers
} // namespace one
