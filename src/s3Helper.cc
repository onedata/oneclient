/**
 * @file s3Helper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"

#include <aws/core/auth/AWSCredentialsProvider.h>
#include <aws/core/client/ClientConfiguration.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/GetObjectRequest.h>
#include <aws/s3/model/PutObjectRequest.h>

#include <boost/algorithm/string.hpp>
#include <glog/stl_logging.h>

#include <sstream>

namespace one {
namespace helpers {

constexpr auto RANGE_DELIMITER = "-";

S3Helper::S3Helper(std::unordered_map<std::string, std::string> args)
    : m_args{std::move(args)}
{
}

CTXPtr S3Helper::createCTX(std::unordered_map<std::string, std::string> params)
{
    return std::make_shared<S3HelperCTX>(std::move(params), m_args);
}

asio::mutable_buffer S3Helper::getObject(
    CTXPtr rawCTX, std::string key, asio::mutable_buffer buf, off_t offset)
{
    auto ctx = getCTX(std::move(rawCTX));

    Aws::S3::Model::GetObjectRequest request{};
    request.SetBucket(ctx->getBucket());
    request.SetKey(key);
    request.SetRange(rangeToString(
        offset, static_cast<off_t>(offset + asio::buffer_size(buf) - 1)));
    request.SetResponseStreamFactory([&]() {
        auto stream = new std::stringstream{};
        stream->rdbuf()->pubsetbuf(
            asio::buffer_cast<char *>(buf), asio::buffer_size(buf));
        stream->rdbuf()->pubseekpos(0);
        return stream;
    });

    auto outcome = ctx->getClient()->GetObject(request);
    throwOnError("GetObject", outcome);

//    LOG(ERROR) << "getObject: '" << key << "', content: '"
//               << std::string{asio::buffer_cast<char *>(buf),
//                      static_cast<std::size_t>(
//                          outcome.GetResult().GetContentLength())}
//               << "'";

    return asio::buffer(
        buf, static_cast<std::size_t>(outcome.GetResult().GetContentLength()));
}

std::size_t S3Helper::putObject(
    CTXPtr rawCTX, std::string key, asio::const_buffer buf)
{
    auto ctx = getCTX(std::move(rawCTX));

    Aws::S3::Model::PutObjectRequest request{};
    auto size = asio::buffer_size(buf);
    auto stream = std::make_shared<std::stringstream>();
    stream->rdbuf()->pubsetbuf(
        const_cast<char *>(asio::buffer_cast<const char *>(buf)), size);
    stream->rdbuf()->pubseekpos(0);
    request.SetBucket(ctx->getBucket());
    request.SetKey(key);
    request.SetContentLength(size);
    request.SetBody(stream);

//    LOG(ERROR) << "putObject: '" << key << "', content: '" << stream->str()
//               << "'";

    auto outcome = ctx->getClient()->PutObject(request);
    throwOnError("PutObject", outcome);

    return size;
}

void S3Helper::deleteObjects(CTXPtr ctx, std::vector<std::string> keys)
{
    return KeyValueHelper::deleteObjects(ctx, keys);
}

std::vector<std::string> S3Helper::listObjects(CTXPtr ctx, std::string prefix)
{
    return KeyValueHelper::listObjects(ctx, prefix);
}

std::shared_ptr<S3HelperCTX> S3Helper::getCTX(CTXPtr rawCTX) const
{
    auto ctx = std::dynamic_pointer_cast<S3HelperCTX>(rawCTX);
    if (ctx == nullptr) {
        LOG(INFO) << "Helper changed. Creating new context with arguments: "
                  << m_args;
        return std::make_shared<S3HelperCTX>(rawCTX->parameters(), m_args);
    }
    return ctx;
}

std::string S3Helper::rangeToString(off_t lower, off_t upper) const
{
    std::stringstream ss;
    ss << "bytes=" << lower << RANGE_DELIMITER << upper;
    return ss.str();
}

S3HelperCTX::S3HelperCTX(std::unordered_map<std::string, std::string> params,
    std::unordered_map<std::string, std::string> args)
    : IStorageHelperCTX{std::move(params)}
    , m_args{std::move(args)}
{
    m_args.insert({S3_HELPER_ACCESS_KEY_ARG, ""});
    m_args.insert({S3_HELPER_SECRET_KEY_ARG, ""});
    init();
}

void S3HelperCTX::setUserCTX(std::unordered_map<std::string, std::string> args)
{
    m_args.swap(args);
    m_args.insert(args.begin(), args.end());
    init();
}

std::unordered_map<std::string, std::string> S3HelperCTX::getUserCTX()
{
    return {{S3_HELPER_ACCESS_KEY_ARG, m_args.at(S3_HELPER_ACCESS_KEY_ARG)},
        {S3_HELPER_SECRET_KEY_ARG, m_args.at(S3_HELPER_SECRET_KEY_ARG)}};
}

const std::string &S3HelperCTX::getBucket() const
{
    return m_args.at(S3_HELPER_BUCKET_NAME_ARG);
}

const std::unique_ptr<Aws::S3::S3Client> &S3HelperCTX::getClient() const
{
    return m_client;
}

void S3HelperCTX::init()
{
    Aws::Auth::AWSCredentials credentials{m_args.at(S3_HELPER_ACCESS_KEY_ARG),
        m_args.at(S3_HELPER_SECRET_KEY_ARG)};
    Aws::Client::ClientConfiguration configuration;

    auto search = m_args.find(S3_HELPER_SCHEME_ARG);
    if (search != m_args.end() && boost::iequals(search->second, "http"))
        configuration.scheme = Aws::Http::Scheme::HTTP;

    search = m_args.find(S3_HELPER_HOST_NAME_ARG);
    if (search != m_args.end())
        configuration.endpointOverride = search->second;

    m_client = std::make_unique<Aws::S3::S3Client>(credentials, configuration);
}

std::map<Aws::S3::S3Errors, std::errc> S3Helper::s_errors = {
    {Aws::S3::S3Errors::INVALID_PARAMETER_VALUE, std::errc::invalid_argument},
    {Aws::S3::S3Errors::MISSING_ACTION, std::errc::not_supported},
    {Aws::S3::S3Errors::SERVICE_UNAVAILABLE, std::errc::host_unreachable},
    {Aws::S3::S3Errors::NETWORK_CONNECTION, std::errc::network_unreachable},
    {Aws::S3::S3Errors::REQUEST_EXPIRED, std::errc::timed_out},
    {Aws::S3::S3Errors::ACCESS_DENIED, std::errc::permission_denied},
    {Aws::S3::S3Errors::NO_SUCH_BUCKET, std::errc::no_such_file_or_directory},
    {Aws::S3::S3Errors::NO_SUCH_KEY, std::errc::no_such_file_or_directory},
    {Aws::S3::S3Errors::RESOURCE_NOT_FOUND,
        std::errc::no_such_file_or_directory}};

} // namespace helpers
} // namespace one
