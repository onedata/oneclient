/**
 * @file testRunner.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testRunner.h"

#include "cephHelper.h"
#include "nullDeviceHelper.h"
#include "posixHelper.h"
#include "s3Helper.h"
#include "testWorkerRndRd.h"
#include "testWorkerRndWr.h"

#include <folly/Function.h>

#include <iostream>

namespace one {
namespace bench {

TestRunner::TestRunner(TestRunnerConfig config)
    : m_config{config}
    , m_workerPool{100}
    , m_resultsQueue{10000}
    , m_idleWork{asio::make_work_guard(m_service)}
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

    if (m_config.storageType == "ceph") {
        helperFactory =
            std::make_shared<one::helpers::CephHelperFactory>(m_service);
    }
    else if (m_config.storageType == "s3") {
        helperFactory =
            std::make_shared<one::helpers::S3HelperFactory>(m_service);
    }
    else if (m_config.storageType == "null") {
        helperFactory =
            std::make_shared<one::helpers::NullDeviceHelperFactory>(m_service);
    }
    else if (m_config.storageType == "posix") {
        helperFactory =
            std::make_shared<one::helpers::PosixHelperFactory>(m_service);
    }
    else {
        throw std::invalid_argument(
            "Unknown storage type: " + m_config.storageType.toStdString());
    }

    std::cout << "== Creating " << m_config.helperCount
              << " helper instances to " << m_config.storageType
              << " storage ===" << std::endl;

    // Start helper worker threads
    for (int i = 0; i < m_config.helperThreadCount; i++) {
        m_serviceThreads.emplace_back(
            std::thread{[&, this] { m_service.run(); }});
    }

    // Create the helper pool
    m_helperPool.reserve(m_config.helperCount);
    for (decltype(m_config.helperCount) i = 0; i < m_config.helperCount; i++) {
        auto helperPtr =
            helperFactory->createStorageHelper(m_config.helperParams);
        m_helperPool.emplace_back(std::move(helperPtr));
    }

    // Prepare unique file names
    for (decltype(m_config.fileCount) i = 0; i < m_config.fileCount; i++) {
        m_fileIds.emplace_back(folly::fbstring(ONEBENCH_FILE_PREFIX) +
            randStr(ONEBENCH_FILEID_LENGTH));
    }

    createTestFiles();
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
                    m_config.events / m_config.testThreadCount,
                    m_config.asyncBatchSize, m_config.flush);
            m_workerPool.add(std::move(testWorker));
        }
        else if (m_config.testType == "rndrd") {
            folly::Function<void()> testWorker =
                TestWorkerRndRd(i, m_workerPool, m_startBarrier, m_stopBarrier,
                    m_helperPool[i % m_helperPool.size()], m_resultsQueue,
                    m_fileIds, m_config.fileSize, m_config.blockSize,
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

        int resultsCounter = 0;
        int intermediateResultsCounter = 0;
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

    m_service.stop();
    for (auto &t : m_serviceThreads)
        t.join();
}

void TestRunner::createTestFiles()
{
    std::cout << "== Creating test files ===\n";

    auto helper = m_helperPool[0];
    for (auto &testFile : m_fileIds) {
        helper->mknod(testFile, S_IFREG, {}, 0).get();
        helper->truncate(testFile, m_config.fileSize).get();
    }
}

void TestRunner::removeTestFiles()
{
    std::cout << "== Removing test files ===\n";

    auto helper = m_helperPool[0];
    for (auto &testFile : m_fileIds) {
        helper->unlink(testFile).get();
    }
}
}
};
