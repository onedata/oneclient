/**
 * @file testResult.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "common.h"

namespace one {
namespace bench {

struct TestResult {
    TestResult()
        : workerID{0}
        , resultID{0}
        , ioCount{0}
        , dataSize{0}
    {
    }

    TestResult(TestWorkerID workerID_, TestResultID resultID_, TimePoint start_,
        TimePoint end_, int ioCount_, size_t dataSize_)
        : workerID{workerID_}
        , resultID{resultID_}
        , start{start_}
        , end{end_}
        , ioCount{ioCount_}
        , dataSize{dataSize_}
    {
    }

    TestWorkerID workerID;
    TestResultID resultID;
    TimePoint start;
    TimePoint end;
    int ioCount;
    size_t dataSize;
};
}
};
