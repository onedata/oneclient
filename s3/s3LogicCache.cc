/**
* @file s3LogicCache.cc
* @author Bartek Kryza
* @copyright (C) 2022-present Onedata.org
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
 */

#include "s3LogicCache.h"

namespace one {
namespace s3 {

S3LogicCache::S3LogicCache(
    std::shared_ptr<one::client::options::Options> options)
    : m_options{std::move(options)}
    , m_initialized{true}
    , m_executor{std::make_shared<folly::IOThreadPoolExecutor>(
          std::thread::hardware_concurrency())}
{
}

folly::Future<std::shared_ptr<S3Logic>> S3LogicCache::get(
    const folly::fbstring &token)
{
    assert(m_options);

    folly::fbstring effectiveToken = token;
    if (effectiveToken.empty())
        effectiveToken = m_options->getAccessToken().get();

    try {
        std::lock_guard<std::mutex> lock{m_cacheMutex};

        if (m_cache.find(effectiveToken) == m_cache.end()) {
            auto p = std::make_shared<
                folly::SharedPromise<std::shared_ptr<S3Logic>>>();
            m_cache.emplace(effectiveToken, p);

            auto s3LogicPtr = std::make_shared<S3Logic>(
                m_options, effectiveToken, m_executor);

            s3LogicPtr->connect().thenTry(
                [this, effectiveToken, s3LogicPtr, p = std::move(p)](
                    auto &&s3Logic) mutable {
                    if (s3Logic.hasException()) {
                        std::lock_guard<std::mutex> lock{m_cacheMutex};
                        m_cache.erase(effectiveToken);
                        throw one::s3::error::AccessDenied("", "", "");
                    }

                    p->setValue(std::move(s3Logic.value()));
                });
        }

        return m_cache.at(effectiveToken)->getFuture();
    }
    catch (...) {
        std::lock_guard<std::mutex> lock{m_cacheMutex};
        m_cache.erase(effectiveToken);
        return folly::makeFutureWith([]() -> std::shared_ptr<S3Logic> {
            throw one::s3::error::AccessDenied("", "", "");
        });
    }
}
} // namespace s3
} // namespace one
