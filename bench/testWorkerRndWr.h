/**
 * @file testWorkerRndWr.h
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
#include <folly/Unit.h>
#include <folly/executors/GlobalExecutor.h>

#include <iostream>

namespace one {
namespace bench {

class TestWorkerRndWr : public TestWorker {
public:
    TestWorkerRndWr(TestWorkerID id, folly::IOThreadPoolExecutor &executor,
        folly::futures::Barrier &startBarrier,
        folly::futures::Barrier &stopBarrier,
        std::shared_ptr<one::helpers::StorageHelper> helper,
        folly::MPMCQueue<TestResult, std::atomic, true> &resultsQueue,
        const folly::fbvector<folly::fbstring> &fileIds, size_t fileSize,
        size_t blockSize, int eventCount, int asyncBatchSize, bool flush)
        : TestWorker(id, executor, startBarrier, stopBarrier, std::move(helper),
              resultsQueue, fileIds, fileSize, blockSize, eventCount,
              asyncBatchSize, flush)
    {
    }

    virtual void operator()() override
    {
        auto fileCount = m_fileIds.size();

        // Open the files
        folly::fbvector<std::shared_ptr<one::helpers::FileHandle>> handles(
            fileCount);

        for (size_t i = 0; i < fileCount; i++) {
            handles[i] = m_helper->open(m_fileIds[i], O_WRONLY, {}).get();
        }

        // Prepare random data for writing
        auto data = randStr(m_blockSize);

        // Wait for all test workers to be ready
        m_startBarrier.wait().get();

        auto f = [this, id = m_id, fileCount, fileSize = m_fileSize,
                     blockSize = m_blockSize, eventCount = m_eventCount,
                     asyncBatchSize = m_asyncBatchSize, flush = m_flush,
                     data = std::move(data), handles = std::move(handles)]() {
            for (int k = 0; k < eventCount; k += asyncBatchSize) {

                folly::fbvector<folly::Future<folly::Unit>> futs;
                futs.reserve(asyncBatchSize);

                for (int l = 0; l < asyncBatchSize; l++) {
                    auto fileIndex = std::rand() % fileCount;
                    auto offset = std::rand() % (fileSize - blockSize);

                    folly::IOBufQueue buf{
                        folly::IOBufQueue::cacheChainLength()};
                    buf.append(data);

                    auto start = Clock::now();

                    futs.emplace_back(
                        handles[fileIndex]
                            ->write(offset, std::move(buf))
                            .then(folly::getIOExecutor().get(),
                                [this, handle = handles[fileIndex], flush,
                                    resultID = k + l](std::size_t written) {
                                    if (flush)
                                        return handle->flush();
                                    else
                                        return folly::makeFuture();
                                })
                            .then(folly::getIOExecutor().get(),
                                [this, start, blockSize, id,
                                    resultID = k + l]() {
                                    postResult({id, resultID, start,
                                        Clock::now(), 1, blockSize});

                                    return folly::makeFuture();
                                }));
                }
                folly::collectAll(futs.begin(), futs.end()).get();
            }
        };

        f();

        m_stopBarrier.wait().get();
    }
};
}
}
