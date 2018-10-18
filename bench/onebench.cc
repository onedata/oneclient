/**
 * @file main.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "common.h"
#include "helpers/storageHelper.h"
#include "testRunner.h"
#include "testRunnerConfig.h"
#include "version.h"

#include <folly/FBString.h>
#include <folly/String.h>
#include <folly/init/Init.h>
#include <gflags/gflags.h>

#include <iostream>
#include <set>

static const std::set<folly::fbstring> storages{
    "posix", "s3", "ceph", "cephrados", "webdav", "glusterfs", "swift", "null"};

static const std::set<folly::fbstring> tests{"rndwr"};

DEFINE_string(storage, "",
    "Specify storage type: posix, s3, ceph, cephrados, webdav, glusterfs, "
    "swift, null");

DEFINE_string(test, "", "Specify test type: rndwr");

DEFINE_int32(helper_count, 1, "Specify number of helper instances");
DEFINE_int32(helper_threads, 8, "Specify number of helper worker threads");
DEFINE_int32(test_threads, 8, "Specify number of test worker threads");

DEFINE_int32(file_count, 1, "Specify number of test files");
DEFINE_int32(file_size, 1024 * 1024, "Specify maximum size of each file");
DEFINE_int32(block_size, 4096, "Specify the block size used for requests");
DEFINE_int32(events, 1000, "Specify number of IO requests");
DEFINE_int32(report_interval, 10, "Specify report interval in seconds");
DEFINE_int32(async_batch_size, 1, "Specify request batch size for each worker");
DEFINE_bool(keep_test_files, false,
    "Keep created temporary test files after test completes");
DEFINE_bool(flush, false, "Force flush after each IO request");

DEFINE_int32(
    timeout_ms, 60 * 1000, "Specify maximum request timeout in milliseconds");

DEFINE_string(ceph_mon_host, "", "Specify Ceph monitor host");
DEFINE_string(ceph_pool, "", "Specify Ceph pool name");
DEFINE_string(ceph_user, "", "Specify Ceph user name, e.g. 'client.admin'");
DEFINE_string(ceph_key, "", "Specify Ceph user key");
DEFINE_int32(ceph_blocksize, 1024 * 1024 * 4,
    "Specify the block size used for striping files into Ceph objects (for "
    "cephrados only)");

DEFINE_string(s3_host, "", "Specify the S3 host, e.g.: '192.168.1.2'");
DEFINE_string(s3_scheme, "https",
    "Specify whether to use http or https for connecting to S3");
DEFINE_string(
    s3_bucket, "", "Specify the name of the S3 bucket to use for test");
DEFINE_string(s3_accesskey, "", "Specify S3 access key");
DEFINE_string(s3_secretkey, "", "Specify S3 secret key");
DEFINE_int32(s3_blocksize, 1024 * 1024 * 10,
    "Specify the block size used for striping files into S3 objects");

DEFINE_string(
    webdav_endpoint, "", "Specify the WebDAV host, e.g.: 'http://192.168.1.2'");
DEFINE_string(webdav_credentials_type, "basic",
    "Specify the WebDAV credentials, e.g.: 'basic'");
DEFINE_string(webdav_credentials, "",
    "Specify the WebDAV credentials, e.g.: 'admin:password'");
DEFINE_string(webdav_rangewrite, "none",
    "Specify the WebDAV range write support, e.g.: 'sabredav'");
DEFINE_string(webdav_verify_certificate, "false",
    "Specify whether onebench should verify WebDAV server certificate.");
DEFINE_int64(webdav_maximum_upload_size, 0,
    "Specify WebDAV maximum upload size for single PUT or PATCH request");
DEFINE_int32(webdav_connection_pool_size, 10,
    "Specify WebDAV connection pool size for each helper instance");

DEFINE_string(posix_mount_point, "/tmp", "Specify mountpoint for test files");
DEFINE_string(
    posix_uid, std::to_string(getuid()), "Specify user UID for created files");
DEFINE_string(
    posix_gid, std::to_string(getgid()), "Specify user GID for created files");

one::helpers::Params makeHelperParams()
{
    one::helpers::Params params;
    if (FLAGS_storage == "ceph") {
        params["clusterName"] = "ceph";
        params["monitorHostname"] = FLAGS_ceph_mon_host;
        params["poolName"] = FLAGS_ceph_pool;
        params["username"] = FLAGS_ceph_user;
        params["key"] = FLAGS_ceph_key;
        params["timeout"] = std::to_string(FLAGS_timeout_ms);
    }
    else if (FLAGS_storage == "cephrados") {
        params["clusterName"] = "ceph";
        params["monitorHostname"] = FLAGS_ceph_mon_host;
        params["poolName"] = FLAGS_ceph_pool;
        params["username"] = FLAGS_ceph_user;
        params["key"] = FLAGS_ceph_key;
        params["timeout"] = std::to_string(FLAGS_timeout_ms);
        params["blockSize"] = std::to_string(FLAGS_ceph_blocksize);
    }
    else if (FLAGS_storage == "s3") {
        params["scheme"] = FLAGS_s3_scheme;
        params["hostname"] = FLAGS_s3_host;
        params["bucketName"] = FLAGS_s3_bucket;
        params["accessKey"] = FLAGS_s3_accesskey;
        params["secretKey"] = FLAGS_s3_secretkey;
        params["timeout"] = std::to_string(FLAGS_timeout_ms);
        params["blockSize"] = std::to_string(FLAGS_s3_blocksize);
    }
    else if (FLAGS_storage == "webdav") {
        params["endpoint"] = FLAGS_webdav_endpoint;
        params["credentialsType"] = FLAGS_webdav_credentials_type;
        params["credentials"] = FLAGS_webdav_credentials;
        params["rangeWriteSupport"] = FLAGS_webdav_rangewrite;
        params["verifyServerCertificate"] = FLAGS_webdav_verify_certificate;
        params["maximumUploadSize"] =
            std::to_string(FLAGS_webdav_maximum_upload_size);
        params["connectionPoolSize"] =
            std::to_string(FLAGS_webdav_connection_pool_size);
    }
    else if (FLAGS_storage == "posix") {
        params["mountPoint"] = FLAGS_posix_mount_point;
        params["uid"] = FLAGS_posix_uid;
        params["gid"] = FLAGS_posix_gid;
    }
    else if (FLAGS_storage == "null") {
    }
    return params;
}

one::bench::TestRunnerConfig makeTestRunnerConfig()
{
    one::bench::TestRunnerConfig config;

    config.helperParams = makeHelperParams();
    config.storageType = FLAGS_storage;
    config.helperCount = FLAGS_helper_count;
    config.helperThreadCount = FLAGS_helper_threads;
    config.testThreadCount = FLAGS_test_threads;
    config.events = FLAGS_events;
    config.fileCount = FLAGS_file_count;
    config.fileSize = FLAGS_file_size;
    config.blockSize = FLAGS_block_size;
    config.testType = FLAGS_test;
    config.reportInterval = FLAGS_report_interval;
    config.asyncBatchSize = FLAGS_async_batch_size;
    config.keepTestFiles = FLAGS_keep_test_files;
    config.flush = FLAGS_flush;

    return config;
}

int main(int argc, char *argv[])
{
    // Use current time to set rand seed
    std::srand(std::time(nullptr));

    // Setup gflags and process command line options
    gflags::SetUsageMessage("onebench is a benchmark utility for measuring \n"
                            "storage performance when accessed via Onedata \n"
                            "helpers library. \n\nUsage:");
    gflags::SetVersionString(ONECLIENT_VERSION);
    gflags::ParseCommandLineFlags(&argc, &argv, true);
    folly::init(&argc, &argv);
    gflags::ShutDownCommandLineFlags();

    auto config = makeTestRunnerConfig();

    std::cout << "== Benchmark config ===" << std::endl;
    std::cout << config << std::endl;

    // Run the benchmark
    one::bench::TestRunner testRunner(std::move(config));
    testRunner.initialize();
    testRunner.start();
    testRunner.stop();

    return 0;
}
