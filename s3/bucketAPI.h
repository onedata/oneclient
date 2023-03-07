/**
 * @file bucketAPI.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "types.h"

#include "s3Logic.h"

#include <drogon/HttpController.h>

namespace one {
namespace s3 {

using namespace drogon;

class BucketAPI {
public:
    void listBuckets(
        const HttpRequestPtr &req, HttpResponseCallback &&callback);

    void putBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void getBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void headBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void deleteBucket(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket) const;

    void setLogicCache(std::shared_ptr<S3LogicCache> logicCache) {
        m_logicCache = logicCache;
    }

private:
    std::shared_ptr<S3LogicCache> m_logicCache;
};
}
}