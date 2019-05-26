/**
 * @file testRunnerConfig.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testRunnerConfig.h"

#include <iostream>
namespace one {
namespace bench {
std::ostream &operator<<(
    std::ostream &stream, const one::bench::TestRunnerConfig &c)
{
    stream << "  Test type: " << c.testType << '\n';
    stream << "  Helper type: " << c.storageType << '\n';
    stream << "  Helper count: " << c.helperCount << '\n';
    stream << "  Helper thread count: " << c.helperThreadCount << '\n';
    stream << "  Test thread count: " << c.testThreadCount << '\n';
    stream << "  Event count: " << c.events << '\n';
    stream << "  File count: " << c.fileCount << '\n';
    stream << "  File size: " << c.fileSize << '\n';
    stream << "  Block size: " << c.blockSize << '\n';
    stream << "  Report interval: " << c.reportInterval << '\n';
    stream << "  Async batch size: " << c.asyncBatchSize << '\n';
    stream << "  Keep test files: " << (c.keepTestFiles ? "yes" : "no") << '\n';
    stream << "  Force flush: " << (c.flush ? "yes" : "no") << '\n';

    return stream;
}
}
}
