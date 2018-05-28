/**
 * @file fileLocation_test.cc
 * @author Rafal Grzeszczuk
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "folly/Benchmark.h"
#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileLocation.h"

using namespace one::messages::fuse;

BENCHMARK(benchmarkPutBlock)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, 1000, FileBlock{" ", " "});
    folly::doNotOptimizeAway(fileLocation);
}

int main() { folly::runBenchmarks(); }
