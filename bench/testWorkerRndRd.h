/**
 * @file testWorkerRndRd.h
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

#include <folly/MPMCQueue.h>

namespace one {
namespace bench {

class TestWorkerRndRd : public TestWorker {
public:
    TestWorkerRndRd(TestWorkerID id, folly::IOThreadPoolExecutor &executor,
        folly::futures::Barrier &startBarrier,
        folly::futures::Barrier &stopBarrier,
        std::shared_ptr<one::helpers::StorageHelper> helper,
        folly::MPMCQueue<TestResult, std::atomic, true> &resultsQueue,
        const folly::fbvector<folly::fbstring> &fileIds, size_t fileSize,
        size_t blockSize, bool blockAligned, int eventCount, int asyncBatchSize,
        bool flush);

    virtual ~TestWorkerRndRd() = default;

    virtual void operator()() override;
};
}
}
