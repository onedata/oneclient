/**
 * @file testWorkerRndWr.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testWorkerRndWr.h"

namespace one {
namespace bench {

TestWorkerRndWr::TestWorkerRndWr(TestWorkerID id,
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

void TestWorkerRndWr::operator()()
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
                 blockSize = m_blockSize, blockAligned = m_blockAligned,
                 eventCount = m_eventCount, asyncBatchSize = m_asyncBatchSize,
                 flush = m_flush,
                 storageBlockSize = m_helper->blockSize() == 0
                     ? (1U << 31)
                     : m_helper->blockSize(),
                 data = std::move(data), handles = std::move(handles)]() {
        for (int k = 0; k < eventCount; k += asyncBatchSize) {

            folly::fbvector<folly::Future<folly::Unit>> futs;
            futs.reserve(asyncBatchSize);

            for (int l = 0; l < asyncBatchSize; l++) {
                auto fileIndex = std::rand() % fileCount;
                auto offset =
                    getRandomOffset(fileSize, blockSize, blockAligned);

                folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
                buf.wrapBuffer(data.data(), data.size(), storageBlockSize);

                auto start = Clock::now();

                futs.emplace_back(
                    handles[fileIndex]
                        ->write(offset, std::move(buf), {})
                        .thenValue(
                            [this, handle = handles[fileIndex], flush,
                                resultID = k + l](std::size_t &&written) {
                                if (flush)
                                    return handle->flush();
                                else
                                    return folly::makeFuture();
                            })
                        .thenValue([this, start, blockSize, id,
                                       resultID = k + l](auto && /*unit*/) {
                            postResult({id, resultID, start, Clock::now(), 1,
                                blockSize});

                            return folly::makeFuture();
                        }));
            }

            folly::collectAll(futs.begin(), futs.end())
                .via(folly::getIOExecutor().get())
                .thenValue([this, handles, fileSize](auto && /*f*/) {
                    folly::fbvector<folly::Future<folly::Unit>> futs;
                    for (auto &h : handles) {
                        futs.emplace_back(
                            m_helper->flushBuffer(h->fileId(), fileSize));
                    }
                    return folly::collectAll(futs.begin(), futs.end())
                        .via(folly::getIOExecutor().get());
                })
                .get();
        }
    };

    f();

    m_stopBarrier.wait().get();
}
} // namespace bench
} // namespace one
