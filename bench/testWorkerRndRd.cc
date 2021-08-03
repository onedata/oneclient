/**
 * @file testWorkerRndRd.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testWorkerRndRd.h"

#include <folly/FBVector.h>
#include <folly/Unit.h>

namespace one {
namespace bench {

TestWorkerRndRd::TestWorkerRndRd(TestWorkerID id,
    folly::IOThreadPoolExecutor &executor,
    folly::futures::Barrier &startBarrier, folly::futures::Barrier &stopBarrier,
    std::shared_ptr<one::helpers::StorageHelper> helper,
    folly::MPMCQueue<TestResult, std::atomic, true> &resultsQueue,
    const folly::fbvector<folly::fbstring> &fileIds, size_t fileSize,
    size_t blockSize, bool blockAligned, int eventCount, int asyncBatchSize,
    bool flush)
    : TestWorker(id, executor, startBarrier, stopBarrier, std::move(helper),
          resultsQueue, fileIds, fileSize, blockSize, blockAligned, eventCount,
          asyncBatchSize, flush)
{
}

void TestWorkerRndRd::operator()()
{
    auto fileCount = m_fileIds.size();

    // Open the files
    folly::fbvector<std::shared_ptr<one::helpers::FileHandle>> handles(
        fileCount);

    for (size_t i = 0; i < fileCount; i++) {
        handles[i] = m_helper->open(m_fileIds[i], O_RDONLY, {}).get();
    }

    // Wait for all test workers to be ready
    m_startBarrier.wait().get();

    auto f = [this, id = m_id, fileCount, fileSize = m_fileSize,
                 blockSize = m_blockSize, blockAligned = m_blockAligned,
                 eventCount = m_eventCount, asyncBatchSize = m_asyncBatchSize,
                 flush = m_flush, handles = std::move(handles)]() {
        for (int k = 0; k < eventCount; k += asyncBatchSize) {

            folly::fbvector<folly::Future<folly::Unit>> futs;
            futs.reserve(asyncBatchSize);

            for (int l = 0; l < asyncBatchSize; l++) {
                auto fileIndex = std::rand() % fileCount;
                auto offset =
                    getRandomOffset(fileSize, blockSize, blockAligned);

                auto start = Clock::now();

                futs.emplace_back(
                    handles[fileIndex]
                        ->read(offset, blockSize)
                        .thenValue(
                            [this, start, blockSize, id, resultID = k + l](
                                folly::IOBufQueue &&buf) {
                                postResult({id, resultID, start, Clock::now(),
                                    1, buf.chainLength()});

                                return folly::makeFuture();
                            }));
            }
            folly::collectAll(futs.begin(), futs.end()).get();
        }
    };

    f();

    m_stopBarrier.wait().get();
}
} // namespace bench
} // namespace one
