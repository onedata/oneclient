/**
 * @file testRunner.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "common.h"
#include "testResult.h"
#include "testRunnerConfig.h"
#include "testWorker.h"

#include <asio.hpp>
#include <folly/FBVector.h>
#include <folly/MPMCQueue.h>
#include <folly/executors/IOThreadPoolExecutor.h>
#include <folly/futures/Barrier.h>

namespace one {
namespace bench {

class TestRunner {

public:
    TestRunner(TestRunnerConfig config);

    void initialize();

    void start();

    void stop();

    void createTestFiles();

    void removeTestFiles();

private:
    TestRunnerConfig m_config;
    folly::fbvector<std::shared_ptr<one::helpers::StorageHelper>> m_helperPool;
    folly::IOThreadPoolExecutor m_workerPool;

    // Queue which serializes results coming from worker threads and puts them
    // in the m_results map for later processing
    folly::MPMCQueue<TestResult, std::atomic, true> m_resultsQueue;

    // Map contains results indexed by worker ids. It is only modified
    // from the thread collecting items from the m_resultsQueue
    std::unordered_map<TestWorkerID, folly::fbvector<TestResult>> m_results;

    asio::io_service m_service;
    folly::fbvector<std::thread> m_serviceThreads;
    asio::executor_work_guard<asio::io_service::executor_type> m_idleWork;

    folly::fbvector<folly::fbstring> m_fileIds;

    folly::futures::Barrier m_startBarrier;
    folly::futures::Barrier m_stopBarrier;
    std::atomic_bool m_stopped;
};
}
};
