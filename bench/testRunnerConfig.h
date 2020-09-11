/**
 * @file testRunnerConfig.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "helpers/storageHelper.h"

#include <iostream>

namespace one {
namespace bench {

struct TestRunnerConfig {
    folly::fbstring testType;
    folly::fbstring storageType;
    int helperCount;
    int helperThreadCount;
    int testThreadCount;
    int events;
    size_t fileCount;
    size_t fileSize;
    size_t blockSize;
    one::helpers::Params helperParams;
    int reportInterval;
    int asyncBatchSize;
    bool keepTestFiles;
    bool flush;
    folly::fbstring fileIndexPath;

private:
    friend std::ostream &operator<<(
        std::ostream &stream, const TestRunnerConfig &c);
};
}
}
