/**
 * @file testRunner.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testRunner.h"

#if WITH_CEPH
#include "cephHelper.h"
#include "cephRadosHelper.h"
#endif
#include "nullDeviceHelper.h"
#include "posixHelper.h"
#if WITH_S3
#include "s3Helper.h"
#endif
#if WITH_WEBDAV
#include "httpHelper.h"
#include "webDAVHelper.h"
#endif
#if WITH_XROOTD
#include "xrootdHelper.h"
#endif
#if WITH_NFS
#include "nfsHelper.h"
#endif

#include "bufferedStorageHelper.h"
#include "storageRouterHelper.h"
#include "testWorkerRndRd.h"
#include "testWorkerRndWr.h"

#include <folly/Function.h>

#include <fstream>
#include <iostream>

namespace one {
namespace bench {

TestRunner::TestRunner(TestRunnerConfig config)
    : m_config{config}
    , m_workerPool{1}
    , m_resultsQueue{10000}
    , m_startBarrier{static_cast<uint32_t>(m_config.testThreadCount + 1)}
    , m_stopBarrier{static_cast<uint32_t>(m_config.testThreadCount + 1)}
    , m_stopped{true}
{
}

void TestRunner::initialize()
{
    std::cout << "== Initializing TestRunner ===" << std::endl;

    // Initialize the test thread worker pool, add one additional thread
    // for results collector thread
    m_workerPool.setNumThreads(m_config.testThreadCount + 1);

    std::shared_ptr<one::helpers::StorageHelperFactory> helperFactory;

    // Start helper worker threads
    m_ioExecutor = std::make_shared<folly::IOThreadPoolExecutor>(
        m_config.helperThreadCount);

    if (m_config.storageType == "null") {
        helperFactory = std::make_shared<one::helpers::NullDeviceHelperFactory>(
            m_ioExecutor);
    }
#if WITH_CEPH
    else if (m_config.storageType == "ceph") {
        helperFactory =
            std::make_shared<one::helpers::CephHelperFactory>(m_ioExecutor);
    }
    else if (m_config.storageType == "cephrados") {
        helperFactory = std::make_shared<one::helpers::CephRadosHelperFactory>(
            m_ioExecutor);
    }
#endif
#if WITH_S3
    else if (m_config.storageType == "s3") {
        helperFactory =
            std::make_shared<one::helpers::S3HelperFactory>(m_ioExecutor);
    }
#endif
#if WITH_WEBDAV
    else if (m_config.storageType == "webdav") {
        helperFactory =
            std::make_shared<one::helpers::WebDAVHelperFactory>(m_ioExecutor);
    }
    else if (m_config.storageType == "http") {
        helperFactory =
            std::make_shared<one::helpers::HTTPHelperFactory>(m_ioExecutor);
    }
#endif
#if WITH_XROOTD
    else if (m_config.storageType == "xrootd") {
        helperFactory =
            std::make_shared<one::helpers::XRootDHelperFactory>(m_ioExecutor);
    }
#endif
#if WITH_NFS
    else if (m_config.storageType == "nfs") {
        helperFactory =
            std::make_shared<one::helpers::NFSHelperFactory>(m_ioExecutor);
    }
#endif
    else if (m_config.storageType == "posix") {
        helperFactory =
            std::make_shared<one::helpers::PosixHelperFactory>(m_ioExecutor);
    }
    else {
        throw std::invalid_argument(
            "Unknown storage type: " + m_config.storageType.toStdString());
    }

    std::cout << "== Creating " << m_config.helperCount
              << " helper instances to " << m_config.storageType
              << " storage ===" << std::endl;

    // Create the helper pool
    m_helperPool.reserve(m_config.helperCount);
    for (auto i = 0; i < m_config.helperCount; i++) {
        if (!m_config.archiveStorage) {
            auto helperPtr =
                helperFactory->createStorageHelper(m_config.helperParams,
                    one::helpers::ExecutionContext::ONECLIENT);
            m_helperPool.emplace_back(std::move(helperPtr));
        }
        else {
            const auto kDefaultBufferStorageBlockSizeMultiplier = 5UL;
            const auto &args = m_config.helperParams;
            auto bufferArgs{args};
            auto mainArgs{args};
            auto bufferedArgs{args};
            bufferArgs["blockSize"] =
                std::to_string(kDefaultBufferStorageBlockSizeMultiplier *
                    std::stoull(args.at("blockSize").c_str()));
            mainArgs["storagePathType"] = "canonical";
            bufferedArgs["bufferPath"] = ".__onedata_buffer";
            bufferedArgs["bufferDepth"] = "2";

            auto bufferHelper =
                one::helpers::S3HelperFactory{m_ioExecutor}.createStorageHelper(
                    bufferArgs, one::helpers::ExecutionContext::ONECLIENT);
            auto mainHelper =
                one::helpers::S3HelperFactory{m_ioExecutor}.createStorageHelper(
                    mainArgs, one::helpers::ExecutionContext::ONECLIENT);
            auto bufferedHelper =
                one::helpers::BufferedStorageHelperFactory{}
                    .createStorageHelper(std::move(bufferHelper),
                        std::move(mainHelper), bufferedArgs,
                        one::helpers::ExecutionContext::ONECLIENT);

            std::map<folly::fbstring, one::helpers::StorageHelperPtr> routes;
            routes["/.__onedata_archive"] = std::move(bufferedHelper);
            routes["/"] =
                one::helpers::S3HelperFactory{m_ioExecutor}.createStorageHelper(
                    args, one::helpers::ExecutionContext::ONECLIENT);

            auto helperPtr =
                std::make_shared<one::helpers::StorageRouterHelper>(
                    std::move(routes),
                    one::helpers::ExecutionContext::ONECLIENT);

            m_helperPool.emplace_back(std::move(helperPtr));
        }
    }

    if (m_config.fileIndexPath.empty()) {
        // Prepare unique file names
        for (auto i = 0u; i < m_config.fileCount; i++) {
            if (m_config.archiveStorage)
                m_fileIds.emplace_back(
                    folly::fbstring("/space1/.__onedata_archive/") +
                    folly::fbstring(ONEBENCH_FILE_PREFIX) +
                    randStr(ONEBENCH_FILEID_LENGTH));
            else
                m_fileIds.emplace_back(folly::fbstring(ONEBENCH_FILE_PREFIX) +
                    randStr(ONEBENCH_FILEID_LENGTH));
        }

        if (m_config.createTestFiles)
            createTestFiles();
    }
    else {
        // Load the file paths from the file
        // The file must contain one path relative to the registered storage
        // per line.
        folly::fbstring filePath{};
        std::ifstream infile(m_config.fileIndexPath.toStdString());
        while (infile >> filePath) {
            m_fileIds.emplace_back(filePath);
            filePath.clear();
        }
    }
}

void TestRunner::start()
{
    std::cout << "== Initializing workers ===" << std::endl;

    m_stopped = false;

    // Create the test workers
    for (int i = 0; i < m_config.testThreadCount; i++) {
        // Create the worker instances:
        // - use consecutive numbers to identify test workers
        // - assign helper instance in a round robin fashion from the
        //   helper pool to worker instances
        // - results queue is shared with all workers who push
        //   intermediate results concurrently
        if (m_config.testType == "rndwr") {
            folly::Function<void()> testWorker =
                TestWorkerRndWr(i, m_workerPool, m_startBarrier, m_stopBarrier,
                    m_helperPool[i % m_helperPool.size()], m_resultsQueue,
                    m_fileIds, m_config.fileSize, m_config.blockSize,
                    m_config.blockAligned,
                    m_config.events / m_config.testThreadCount,
                    m_config.asyncBatchSize, m_config.flush);
            m_workerPool.add(std::move(testWorker));
        }
        else if (m_config.testType == "rndrd") {
            folly::Function<void()> testWorker =
                TestWorkerRndRd(i, m_workerPool, m_startBarrier, m_stopBarrier,
                    m_helperPool[i % m_helperPool.size()], m_resultsQueue,
                    m_fileIds, m_config.fileSize, m_config.blockSize,
                    m_config.blockAligned,
                    m_config.events / m_config.testThreadCount,
                    m_config.asyncBatchSize, m_config.flush);
            m_workerPool.add(std::move(testWorker));
        }
        else {
            throw std::invalid_argument(
                "Unknown test type: " + m_config.testType.toStdString());
        }
    }

    // Start results consumer task
    m_workerPool.add([&, this]() {
        using namespace std::chrono;

        auto resultsCounter = 0;
        auto intermediateResultsCounter = 0;
        auto reportIntervalStart = Clock::now();

        while (!m_stopped) {
            TestResult result;

            if (m_resultsQueue.tryReadUntil(
                    Clock::now() + milliseconds(250), result)) {

                m_results[result.workerID].emplace_back(result);

                auto intervalTime =
                    duration_cast<seconds>(Clock::now() - reportIntervalStart)
                        .count();

                intermediateResultsCounter += result.ioCount;

                if (intervalTime > m_config.reportInterval) {
                    auto poolStats = m_workerPool.getPoolStats();
                    std::cout << "  Completed " << resultsCounter
                              << " requests, IOPS: "
                              << (double)intermediateResultsCounter /
                            (double)intervalTime
                              << ", active test threads: "
                              << poolStats.threadCount - 1 << '\n';
                    reportIntervalStart = Clock::now();
                    intermediateResultsCounter = 0;
                }

                resultsCounter += result.ioCount;
            }
        }
        std::cout << "== Stopping results consumer ===\n";
    });

    m_startBarrier.wait().get();

    auto start = Clock::now();

    std::cout << "== Workers started ===" << std::endl;

    m_stopBarrier.wait().get();

    auto end = Clock::now();

    std::cout << "== Workers completed ===" << std::endl;

    m_stopped = true;

    m_workerPool.stop();

    auto totalTime =
        std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    std::cout << "== Test Completed ===\n";
    std::cout << "  Total events: " << m_config.events << "\n";
    std::cout << "  Total time [ms]: " << totalTime.count() << "\n";
    std::cout << "  IOPS: "
              << (double)m_config.events / ((double)totalTime.count() / 1000.0)
              << "\n";
    std::cout << "  Throughput [MB/s]: "
              << (double)(m_config.events * m_config.blockSize /
                     (1024.0 * 1024.0)) /
            ((double)totalTime.count() / 1000.0)
              << "\n";
}

void TestRunner::stop()
{
    if (!m_config.keepTestFiles)
        removeTestFiles();

    std::cout << "== Stopping TestRunner ===" << std::endl;
}

void TestRunner::createTestFiles()
{
    std::cout << "== Creating test files ===\n";

    auto helper = m_helperPool[0];
    for (auto &testFile : m_fileIds) {
        helper->mknod(testFile, 0644, one::helpers::maskToFlags(S_IFREG), 0)
            .get();
        helper->truncate(testFile, m_config.fileSize, 0).get();
    }
}

void TestRunner::removeTestFiles()
{
    std::cout << "== Removing test files ===\n";

    auto helper = m_helperPool[0];
    for (auto &testFile : m_fileIds) {
        helper->unlink(testFile, m_config.fileSize).get();
    }
}
}
};
