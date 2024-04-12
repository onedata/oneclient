/**
 * @file s3server.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "bucketAPI.h"
#include "oneproviderRestClient.h"
#include "onezoneRestClient.h"
#include "s3LogicCache.h"
#include "types.h"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <drogon/HttpController.h>

using namespace drogon;
namespace one {
namespace s3 {

struct S3Authorization {
    virtual ~S3Authorization() = default;

    virtual std::string getToken() const = 0;

    static std::unique_ptr<S3Authorization> fromHttpRequest(
        const HttpRequestPtr &req);
};

struct S3AuthorizationNone final : public S3Authorization {
    std::string getToken() const override { return "__ANONYMOUS__"; }
};

struct S3AuthorizationInvalid final : public S3Authorization {
    std::string getToken() const override { return "__INVALID__"; }
};

struct S3AuthorizationV4 : public S3Authorization {

    std::string getToken() const override { return accessKeyId; }

    bool parseCredential(const std::string &credential);

    // E.g. AWS4-HMAC-SHA256
    std::string algorithm;
    // The users access key id
    std::string accessKeyId;
    // The date of the signature
    std::string date;
    // Region used to generate the signature
    std::string region;
    // This must be s3
    std::string service;
    // The list of headers from which the signature was obtained
    std::vector<std::string> signedHeaders;
    // Signature value
    std::string signature;
};

class S3Server : public drogon::HttpController<S3Server, false> {
public:
    METHOD_LIST_BEGIN
    //
    // Bucket methods
    //
    ADD_METHOD_TO(S3Server::listBuckets, "/", Get);
    ADD_METHOD_TO(S3Server::putBucket, "/{bucket}", Put);
    //    ADD_METHOD_TO(S3Server::getBucket, "/{bucket}", Get);
    ADD_METHOD_TO(S3Server::headBucket, "/{bucket}", Head);
    ADD_METHOD_TO(S3Server::deleteBucket, "/{bucket}", Delete);

    //
    // Object methods
    //
    ADD_METHOD_TO(S3Server::getObject, "/{bucket}/{file}", {Get, Head});
    ADD_METHOD_TO(S3Server::getObject, "/{bucket}/(.*)", {Get, Head});
    ADD_METHOD_TO(S3Server::putObject, "/{bucket}/{file}", Put);
    ADD_METHOD_TO(S3Server::putObject, "/{bucket}/(.*)", Put);
    ADD_METHOD_TO(S3Server::deleteObject, "/{bucket}/{file}", Delete);
    ADD_METHOD_TO(S3Server::deleteObject, "/{bucket}/(.*)", Delete);
    ADD_METHOD_TO(S3Server::getBucket, "/{bucket}", Get);
    ADD_METHOD_TO(S3Server::headBucket, "/{bucket}", Head);

    //
    // Multipart upload methods
    //
    ADD_METHOD_TO(S3Server::postObject, "/{bucket}/{file}", Post);
    ADD_METHOD_TO(S3Server::postObject, "/{bucket}/(.*)", Post);

    //
    // Delete multiple objects
    //
    ADD_METHOD_TO(S3Server::postBucket, "/{bucket}", Post);

    //
    // Readiness probe endpoint
    //
    ADD_METHOD_TO(S3Server::readinessProbe, "/.__onedata__status__", Get);

    METHOD_LIST_END

    S3Server() = delete;

    S3Server(std::shared_ptr<one::client::options::Options> options)
        : m_logicCache{std::make_shared<S3LogicCache>(options)}
        , m_options{options}
        , m_readinessProbeBasicAuth{
              m_options->getOneS3ReadinessProbeBasicAuth()}
    {
        m_randomGenerator.seed(std::random_device{}());
        m_uuidGenerator = boost::uuids::basic_random_generator<std::mt19937>{
            m_randomGenerator};
    }

    void listBuckets(
        const HttpRequestPtr &req, HttpResponseCallback &&callback);

    void putBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void postBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void getBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void headBucket(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void deleteBucket(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket) const;

    void getLocationConstraint(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket) const;

    void getVersioning(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket) const;

    void headObject(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket, const std::string &path) const;

    void getObject(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket, const std::string &path) const;

    void putObject(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket, const std::string &path) const;

    void putMultipartPart(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void putCompleteObject(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void deleteObject(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void deleteObjects(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket) const;

    void listObjects(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket) const;

    void postObject(const HttpRequestPtr &req, HttpResponseCallback &&callback,
        const std::string &bucket, const std::string &path) const;

    void createMultipartUpload(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void uploadMultipartUpload(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void abortMultipartUpload(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void completeMultipartUpload(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void listMultipartUploadParts(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void listMultipartUploads(const HttpRequestPtr &req,
        HttpResponseCallback &&callback, const std::string &bucket,
        const std::string &path) const;

    void readinessProbe(
        const HttpRequestPtr &req, HttpResponseCallback &&callback);

    void setLogicCache(std::shared_ptr<S3LogicCache> logicCache)
    {
        m_logicCache = logicCache;
    }

private:
    std::string getRequestId() const;

    bool bucketNameCached(const std::string &name) const;

    std::string getCachedBucketId(const std::string &name) const;

    void cacheBucketName(const std::string &name, const std::string &id) const;

    bool ensureSpaceIsSupported(const std::string &bucket,
        const HttpResponseCallback &callback, const std::string &requestId,
        const std::string &token, bool emptyBodyOn404 = false) const;

    std::string toMetricName(
        const std::string &op, const std::string &bucket) const;

    void setOnepanelCredentials(const std::string &bucket,
        const std::string &requestId,
        rest::onepanel::OnepanelClient &onepanelClient) const;

    void checkIfSpaceExistsInOnezone(const std::string &bucket,
        const std::string &requestId, const std::string &token,
        one::rest::onezone::OnezoneClient &onezoneClient,
        one::rest::oneprovider::OneproviderClient &oneproviderClient) const;

    bool waitUntilSpaceIsVisibleInS3Logic(const std::string &bucket,
        const std::string &spaceId, const std::string &token) const;

    void handleGetObjectStreamResponse(
        Aws::S3::Model::GetObjectResult &getResult,
        std::function<std::size_t(char *, std::size_t)> handler,
        const std::string &path, HttpResponseCallback &callback) const
    {
        auto response = HttpResponse::newStreamResponse(
            handler, path, CT_NONE, getResult.GetContentType());

        response->setContentTypeString(getResult.GetContentType());

        if (!getResult.GetContentRange().empty()) {
            response->addHeader("content-range", getResult.GetContentRange());
            response->setStatusCode(drogon::HttpStatusCode::k206PartialContent);
        }

        response->addHeader(
            "content-length", std::to_string(getResult.GetContentLength()));
        response->addHeader("etag", getResult.GetETag());
        response->addHeader("accept-ranges", "bytes");
        response->addHeader("last-modified",
            getResult.GetLastModified().ToGmtString(
                Aws::Utils::DateFormat::RFC822));

        callback(response);
    }

    void handleGetObjectResponse(Aws::S3::Model::GetObjectResult &getResult,
        std::string body, const std::string &path,
        HttpResponseCallback &callback) const
    {
        auto response = HttpResponse::newHttpResponse();

        response->setContentTypeString(getResult.GetContentType());

        if (!getResult.GetContentRange().empty()) {
            response->addHeader("content-range", getResult.GetContentRange());
            response->setStatusCode(drogon::HttpStatusCode::k206PartialContent);
        }
        response->addHeader(
            "content-length", std::to_string(getResult.GetContentLength()));
        response->addHeader("etag", getResult.GetETag());
        response->addHeader("accept-ranges", "bytes");
        response->addHeader("last-modified",
            getResult.GetLastModified().ToGmtString(
                Aws::Utils::DateFormat::RFC822));

        response->setBody(std::move(body));

        response->addHeader(
            "Content-Disposition", "attachment; filename=" + path);

        callback(response);
    }

    std::shared_ptr<S3LogicCache> m_logicCache;
    std::shared_ptr<one::client::options::Options> m_options;

    const boost::optional<std::string> m_readinessProbeBasicAuth;
    const boost::optional<boost::filesystem::path> m_sslCertPath;
    const boost::optional<boost::filesystem::path> m_sslKeyPath;

    const boost::optional<unsigned int> m_httpPort;
    const boost::optional<unsigned int> m_httpsPort;

    mutable std::mutex m_uuidGeneratorMutex;
    std::mt19937 m_randomGenerator;
    mutable boost::uuids::basic_random_generator<std::mt19937> m_uuidGenerator;

    mutable folly::ConcurrentHashMap<std::string /* bucketName */,
        std::string /* spaceId */>
        m_bucketNameCache;
};

} // namespace s3
} // namespace one