/**
 * @file s3Helper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_S3_HELPER_H
#define HELPERS_S3_HELPER_H

#include "asioExecutor.h"
#include "keyValueAdapter.h"
#include "keyValueHelper.h"

#include <aws/core/Aws.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/S3Errors.h>

#include <map>
#include <sstream>

namespace Aws {
namespace S3 {
class S3Client;
}
}

namespace one {
namespace helpers {

class S3Helper;

/**
 * An implementation of @c StorageHelperFactory for S3 storage helper.
 */
class S3HelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param service @c io_service that will be used for some async operations.
     */
    S3HelperFactory(asio::io_service &service)
        : m_service{service}
    {
    }

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        const auto &scheme = getParam(parameters, "scheme", "https");
        const auto &hostname = getParam(parameters, "host_name");
        const auto &bucketName = getParam(parameters, "bucket_name");
        const auto &accessKey = getParam(parameters, "access_key");
        const auto &secretKey = getParam(parameters, "secret_key");
        const auto &blockSize =
            getParam<std::size_t>(parameters, "block_size", DEFAULT_BLOCK_SIZE);

        return std::make_shared<KeyValueAdapter>(
            std::make_shared<S3Helper>(
                hostname, bucketName, accessKey, secretKey, scheme == "https"),
            std::make_shared<AsioExecutor>(m_service), blockSize);
    }

private:
    asio::io_service &m_service;
};

/**
* The S3Helper class provides access to Simple Storage Service (S3) via AWS SDK.
*/
class S3Helper : public KeyValueHelper {
public:
    /**
     * Constructor.
     * @param hostName Hostname of the S3 server.
     * @param bucketName Name of the used S3 bucket.
     * @param accessKey Access key of the S3 user.
     * @param secretKey Secret key of the S3 user.
     * @param useHttps Determines whether to use https or http connection.
     */
    S3Helper(folly::fbstring hostName, folly::fbstring bucketName,
        folly::fbstring accessKey, folly::fbstring secretKey,
        const bool useHttps = true);

    folly::IOBufQueue getObject(const folly::fbstring &key, const off_t offset,
        const std::size_t size) override;

    off_t getObjectsSize(
        const folly::fbstring &prefix, const std::size_t objectSize) override;

    std::size_t putObject(
        const folly::fbstring &key, folly::IOBufQueue buf) override;

    void deleteObjects(const folly::fbvector<folly::fbstring> &keys) override;

    folly::fbvector<folly::fbstring> listObjects(
        const folly::fbstring &prefix) override;

private:
    folly::fbstring m_bucket;
    std::unique_ptr<Aws::S3::S3Client> m_client;
};

/*
 * The S3HelperApiInit class is responsible for initialization and cleanup of
 * AWS SDK C++ library. It should be instantiated prior to any library call.
 */
class S3HelperApiInit {
public:
    S3HelperApiInit() { Aws::InitAPI(m_options); }

    ~S3HelperApiInit() { Aws::ShutdownAPI(m_options); }

private:
    Aws::SDKOptions m_options;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_S3_HELPER_H
