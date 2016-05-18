/**
 * @file s3Helper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"
#include "logging.h"

#include <glog/stl_logging.h>

#include <algorithm>
#include <ctime>
#include <functional>
#include <sstream>
#include <vector>

namespace one {
namespace helpers {

S3Helper::S3Helper(std::unordered_map<std::string, std::string> args)
    : m_args{std::move(args)}
{
}

CTXPtr S3Helper::createCTX(std::unordered_map<std::string, std::string> params)
{
    return std::make_shared<S3HelperCTX>(std::move(params), m_args);
}

asio::mutable_buffer S3Helper::getObject(
    CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset)
{
    return KeyValueHelper::getObject(ctx, key, buf, offset);
}

std::size_t S3Helper::putObject(
    CTXPtr ctx, std::string key, asio::const_buffer buf)
{
    return KeyValueHelper::putObject(ctx, key, buf);
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

S3HelperCTX::S3HelperCTX(std::unordered_map<std::string, std::string> params,
    std::unordered_map<std::string, std::string> args)
    : IStorageHelperCTX{std::move(params)}
    , m_args{std::move(args)}
{
}

void S3HelperCTX::setUserCTX(std::unordered_map<std::string, std::string> args)
{
    m_args.swap(args);
    m_args.insert(args.begin(), args.end());
}

std::unordered_map<std::string, std::string> S3HelperCTX::getUserCTX()
{
    return {{S3_HELPER_ACCESS_KEY_ARG, m_args.at(S3_HELPER_ACCESS_KEY_ARG)},
        {S3_HELPER_SECRET_KEY_ARG, m_args.at(S3_HELPER_SECRET_KEY_ARG)}};
}

} // namespace helpers
} // namespace one
