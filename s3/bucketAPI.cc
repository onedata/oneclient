/**
 * @file bucketAPI.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "bucketAPI.h"

#include "serialization.h"

#include <aws/s3/model/ListBucketsResult.h>

namespace one {
namespace s3 {

using namespace drogon;

void BucketAPI::listBuckets(
    const HttpRequestPtr &req, HttpResponseCallback &&callback)
{
    m_logicCache->get({}).thenValue(
        [&req, callback = std::move(callback)](std::shared_ptr<S3Logic> &&s3) {
            auto response = HttpResponse::newHttpResponse();
            response->setBody(serialize<Aws::S3::Model::ListBucketsResult>(
                s3->listBuckets()));
            callback(response);
        });
}

void BucketAPI::putBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
}

void BucketAPI::getBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
}

void BucketAPI::headBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
}

void BucketAPI::deleteBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
}

} // namespace s3
} // namespace one