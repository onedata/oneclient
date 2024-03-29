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

static const std::set<folly::fbstring> storages{"posix", "s3", "ceph",
    "cephrados", "webdav", "glusterfs", "swift", "xrootd", "null"};

static const std::set<folly::fbstring> tests{"rndwr"};

DEFINE_string(storage, "",
    "Specify storage type: posix, s3, ceph, cephrados, webdav, glusterfs, "
    "swift, xrootd, null");

DEFINE_string(test, "", "Specify test type: rndwr");

DEFINE_int32(helper_count, 1, "Specify number of helper instances");
DEFINE_int32(helper_threads, 8, "Specify number of helper worker threads");
DEFINE_int32(test_threads, 8, "Specify number of test worker threads");

DEFINE_int32(file_count, 1, "Specify number of test files");
DEFINE_int64(file_size, 1024 * 1024, "Specify maximum size of each file");
DEFINE_int32(block_size, 4096, "Specify the block size used for requests");
DEFINE_bool(block_aligned, false,
    "Specify whether the read and write operations should be aligned to "
    "block_size boundary");
DEFINE_int32(events, 1000, "Specify number of IO requests");
DEFINE_int32(report_interval, 10, "Specify report interval in seconds");
DEFINE_int32(async_batch_size, 1, "Specify request batch size for each worker");
DEFINE_bool(
    create_test_files, true, "Create test files before running benchmark");
DEFINE_bool(keep_test_files, false,
    "Keep created temporary test files after test completes");
DEFINE_bool(flush, false, "Force flush after each IO request");
DEFINE_string(file_index_path, "",
    "File containing list of files, one file per line, relative to the "
    "registered storage");
DEFINE_string(storage_path_type, "canonical",
    "Specify storage path type: canonical or flat.");

DEFINE_int32(
    timeout_ms, 60 * 1000, "Specify maximum request timeout in milliseconds");

DEFINE_bool(
    archive_storage, false, "Does storage support long-term preservation.");

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

DEFINE_int32(nfs_version, 3, "NFS version <3|4>");
DEFINE_string(nfs_host, "", "NFS host");
DEFINE_string(nfs_volume, "/", "NFS volume");
DEFINE_int32(nfs_uid, 0, "NFS UID");
DEFINE_int32(nfs_gid, 0, "NFS GID");
DEFINE_int64(nfs_readahead, 0, "NFS readahead");
DEFINE_int64(nfs_tcpsyncnt, 0, "NFS TCP syncnt");
DEFINE_bool(nfs_dircache, true, "NFS dircache");
DEFINE_int32(nfs_autoreconnect, 0, "NFS autoreconnect");
DEFINE_int32(nfs_connection_pool_size, 10, "NFS connection pool size");

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

DEFINE_string(
    http_endpoint, "", "Specify the HTTP host, e.g.: 'http://192.168.1.2'");
DEFINE_string(http_credentials_type, "basic",
    "Specify the HTTP credentials, e.g.: 'basic'");
DEFINE_string(http_credentials, "",
    "Specify the HTTP credentials, e.g.: 'admin:password'");
DEFINE_string(http_verify_certificate, "false",
    "Specify whether onebench should verify HTTP server certificate.");
DEFINE_int32(http_connection_pool_size, 10,
    "Specify HTTP connection pool size for each helper instance");

DEFINE_string(xrootd_url, "",
    "Specify the XRootD url, e.g.: 'xrootd://192.168.1.2//data/'");
DEFINE_string(xrootd_credentials_type, "none",
    "Specify the XRootD credentials type: [none, pwd]");
DEFINE_string(xrootd_credentials, "",
    "Specify the XRootD credentials, e.g.: 'admin:password'");

DEFINE_string(posix_mount_point, "/tmp", "Specify mountpoint for test files");
DEFINE_string(
    posix_uid, std::to_string(getuid()), "Specify user UID for created files");
DEFINE_string(
    posix_gid, std::to_string(getgid()), "Specify user GID for created files");

DEFINE_string(
    null_latency_min, "0", "Specify null helper minimum simulated latency");
DEFINE_string(
    null_latency_max, "0", "Specify null helper maximum simulated latency");
DEFINE_string(
    null_timeout_probability, "0.0", "Specify null helper timeout probabilty");
DEFINE_string(
    null_apply_issues_to, "*", "Specify null helper issues operation filter");

one::helpers::Params makeHelperParams()
{
    one::helpers::Params params;
    params["storagePathType"] = FLAGS_storage_path_type;
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
        params["archiveStorage"] = std::to_string(FLAGS_archive_storage);
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
    else if (FLAGS_storage == "http") {
        params["endpoint"] = FLAGS_http_endpoint;
        params["credentialsType"] = FLAGS_http_credentials_type;
        params["credentials"] = FLAGS_http_credentials;
        params["verifyServerCertificate"] = FLAGS_http_verify_certificate;
        params["connectionPoolSize"] =
            std::to_string(FLAGS_http_connection_pool_size);
    }
    else if (FLAGS_storage == "xrootd") {
        params["url"] = FLAGS_xrootd_url;
        params["credentialsType"] = FLAGS_xrootd_credentials_type;
        params["credentials"] = FLAGS_xrootd_credentials;
    }
    else if (FLAGS_storage == "posix") {
        params["mountPoint"] = FLAGS_posix_mount_point;
        params["uid"] = FLAGS_posix_uid;
        params["gid"] = FLAGS_posix_gid;
    }
    else if (FLAGS_storage == "nfs") {
        params["version"] = std::to_string(FLAGS_nfs_version);
        params["host"] = FLAGS_nfs_host;
        params["volume"] = FLAGS_nfs_volume;
        params["uid"] = std::to_string(FLAGS_nfs_uid);
        params["gid"] = std::to_string(FLAGS_nfs_gid);
        params["readAhead"] = std::to_string(FLAGS_nfs_readahead);
        params["tcpSyncnt"] = std::to_string(FLAGS_nfs_tcpsyncnt);
        params["dirCache"] = std::to_string(FLAGS_nfs_dircache);
        params["autoReconnect"] = std::to_string(FLAGS_nfs_autoreconnect);
        params["connectionPoolSize"] =
            std::to_string(FLAGS_nfs_connection_pool_size);
    }
    else if (FLAGS_storage == "null") {
        params["latencyMin"] = FLAGS_null_latency_min;
        params["latencyMax"] = FLAGS_null_latency_max;
        params["timeoutProbability"] = FLAGS_null_timeout_probability;
        params["filter"] = FLAGS_null_apply_issues_to;
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
    config.blockAligned = FLAGS_block_aligned;
    config.testType = FLAGS_test;
    config.reportInterval = FLAGS_report_interval;
    config.asyncBatchSize = FLAGS_async_batch_size;
    config.createTestFiles = FLAGS_create_test_files;
    config.keepTestFiles = FLAGS_keep_test_files;
    config.flush = FLAGS_flush;
    config.fileIndexPath = FLAGS_file_index_path;
    config.archiveStorage = FLAGS_archive_storage;

    if (!config.fileIndexPath.empty()) {
        config.keepTestFiles = true;
    }

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
