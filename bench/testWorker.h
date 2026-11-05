/**
 * @file testWorker.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "common.h"
#include "helpers/storageHelper.h"
#include "testResult.h"
#include "testRunner.h"

#include <folly/FBVector.h>
#include <folly/MPMCQueue.h>
#include <folly/executors/IOThreadPoolExecutor.h>
#include <folly/futures/Barrier.h>

#include <iostream>

namespace one {
namespace bench {

class TestWorker {
public:
    TestWorker(TestWorkerID id, folly::IOThreadPoolExecutor &executor,
        folly::futures::Barrier &startBarrier,
        folly::futures::Barrier &stopBarrier,
        std::shared_ptr<one::helpers::StorageHelper> helper,
        folly::MPMCQueue<TestResult, std::atomic, true> &resultsQueue,
        const folly::fbvector<folly::fbstring> &fileIds, size_t fileSize,
        size_t blockSize, bool blockAligned, int eventCount, int asyncBatchSize,
        bool flush)
        : m_id{id}
        , m_executor{executor}
        , m_startBarrier{startBarrier}
        , m_stopBarrier{stopBarrier}
        , m_helper{std::move(helper)}
        , m_fileIds{fileIds}
        , m_fileSize{fileSize}
        , m_blockSize{blockSize}
        , m_blockAligned{blockAligned}
        , m_eventCount{eventCount}
        , m_asyncBatchSize{asyncBatchSize}
        , m_flush{flush}
        , m_resultsQueue{resultsQueue}
    {
    }

    virtual ~TestWorker() = default;

    void postResult(TestResult &&result)
    {
        m_resultsQueue.blockingWrite(std::forward<TestResult>(result));
    }

    virtual void operator()() = 0;

protected:
    inline off_t getRandomOffset(
        const size_t fileSize, const size_t blockSize, const bool blockAligned)
    {
        if (blockAligned) {
            if (blockSize == 0)
                throw std::runtime_error(
                    "block_size 0 cannot be used with block_aligned flag");

            auto blockNumber = std::rand() % (fileSize / blockSize);
            return blockNumber * blockSize;
        }

        return std::rand() % (fileSize - blockSize);
    }

    TestWorkerID m_id;
    folly::IOThreadPoolExecutor &m_executor;
    folly::futures::Barrier &m_startBarrier;
    folly::futures::Barrier &m_stopBarrier;
    std::shared_ptr<one::helpers::StorageHelper> m_helper;
    const folly::fbvector<folly::fbstring> &m_fileIds;
    size_t m_fileSize;
    size_t m_blockSize;
    bool m_blockAligned;
    int m_eventCount;
    int m_asyncBatchSize;
    bool m_flush;

private:
    folly::MPMCQueue<TestResult, std::atomic, true> &m_resultsQueue;
};
}
}
