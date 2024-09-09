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
          std::thread::hardware_concurrency(),
          std::make_shared<folly::NamedThreadFactory>("S3LWork"))}
{
}

folly::Future<std::shared_ptr<S3Logic>> S3LogicCache::get(
    const folly::fbstring &token)
{
    assert(m_options);

    folly::fbstring effectiveToken = token;

    if (effectiveToken.empty()) {
        if (m_options->getAccessToken().has_value()) {
            effectiveToken = m_options->getAccessToken().get();
        }
        else {
            effectiveToken = "__INVALID__";
        }
    }

    try {
        std::lock_guard<std::mutex> lock{m_cacheMutex};

        if (m_cache.find(effectiveToken) == m_cache.end()) {
            auto p = std::make_shared<
                folly::SharedPromise<std::shared_ptr<S3Logic>>>();
            m_cache.emplace(effectiveToken, p);

            if (effectiveToken == "__INVALID__") {
                p->setException(one::s3::error::AccessDenied("", "", ""));
            }
            else {
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

bool S3LogicCache::updateClientStatus(Poco::JSON::Array &clients)
{
    using one::client::util::md5::md5;

    bool isOk{true};
    std::lock_guard<std::mutex> l{m_cacheMutex};
    for (auto &it : m_cache) {
        const auto key = it.first;
        auto s3Logic = it.second->getFuture();

        // S3Logic is still trying to connect
        if (!s3Logic.isReady())
            continue;

        // S3Logic has never connected to the provider
        if (s3Logic.hasException())
            continue;

        Poco::JSON::Object client;
        const auto sessionId = md5(key.toStdString());
        client.set("id", sessionId);

        client.set("isConnected", s3Logic.value()->isConnected());
        client.set("openFileCount", s3Logic.value()->getOpenFileCount());
        client.set("downloadedBytes", s3Logic.value()->getDownloadedBytes());
        client.set("uploadedBytes", s3Logic.value()->getUploadedBytes());
        client.set("activeWorkerThreads",
            s3Logic.value()->getThreadPoolActiveThreads());

        // Check if S3Logic has lost connection to the provider
        if (!s3Logic.value()->isConnected()) {
            LOG(WARNING) << "Connection to Oneprovider lost for session: "
                         << sessionId;
            isOk = false;
        }

        clients.add(std::move(client));
    }

    return isOk;
}

folly::IOThreadPoolExecutor *S3LogicCache::executor()
{
    if (!m_executor)
        return nullptr;
    return m_executor.get();
}
} // namespace s3
} // namespace one
