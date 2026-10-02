/**
 * @file s3LogicCache.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "s3Logic.h"

namespace one {
namespace s3 {

class S3LogicCache {
public:
    S3LogicCache() = default;

    S3LogicCache(std::string oneproviderId,
        std::shared_ptr<one::client::options::Options> options);

    folly::Future<std::shared_ptr<S3Logic>> get(const folly::fbstring &token);

    bool updateClientStatus(Poco::JSON::Array &clients);

    folly::IOThreadPoolExecutor *executor();

    void stop()
    {
        std::lock_guard<std::mutex> l{m_cacheMutex};

        std::vector<folly::Future<folly::Unit>> futs;

        for (auto &it : m_cache) {
            const auto key = it.first;
            auto s3Logic = it.second->getFuture();

            if (!s3Logic.isReady())
                continue;

            if (s3Logic.hasException())
                continue;

            futs.emplace_back(
                s3Logic.via(m_executor.get()).thenValue([](auto &&s3l) {
                    return s3l->stop();
                }));
        }

        folly::collectAll(futs.begin(), futs.end()).get();
    }

private:
    const std::string m_oneproviderId;
    std::shared_ptr<one::client::options::Options> m_options;
    bool m_initialized{false};

    mutable std::mutex m_cacheMutex;

    using TokenStr = folly::fbstring;
    std::unordered_map<TokenStr,
        std::shared_ptr<folly::SharedPromise<std::shared_ptr<S3Logic>>>>
        m_cache;

    std::shared_ptr<folly::IOThreadPoolExecutor> m_executor;
};

} // namespace s3
} // namespace one