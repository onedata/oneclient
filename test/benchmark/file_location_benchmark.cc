/**
 * @file file_location_benchmark.cc
 * @author Rafal Grzeszczuk
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileLocation.h"

#include <boost/random.hpp>
#include <boost/range/irange.hpp>
#include <folly/Benchmark.h>
#include <folly/Foreach.h>

#include <utility>

using namespace one::messages::fuse;

constexpr auto blockSize = 1024; // 1KB

BENCHMARK(benchmarkPutSingleBlock)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPutHugeBlock)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(
        0, blockSize * blockSize * blockSize, FileBlock{" ", " "});
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPutManyBlocks)
{
    auto fileLocation = FileLocation{};
    FOR_EACH_RANGE(i, 0, 1'000'000)
    {
        fileLocation.putBlock(blockSize * i, blockSize, FileBlock{" ", " "});
    }
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPutBlockRandomly)
{
    auto fileLocation = FileLocation{};
    auto randomBlockNumber = 0;
    BENCHMARK_SUSPEND
    {
        for (auto i : boost::irange(1, 1024 * 1024))
            fileLocation.putBlock(
                2 * i * blockSize, blockSize, FileBlock{" ", " "});

        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(1, 1024 * 1024);
        randomBlockNumber = randomBlock(gen);
    }

    fileLocation.putBlock(randomBlockNumber, blockSize, FileBlock{" ", " "});
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPutManyBlocksRandomly)
{
    auto fileLocation = FileLocation{};
    auto randomBlockNumber = 0;
    BENCHMARK_SUSPEND
    {
        for (auto i : boost::irange(1, 1024))
            fileLocation.putBlock(
                i * blockSize, blockSize, FileBlock{" ", " "});
    }
    FOR_EACH_RANGE(i, 0, 100'000)
    {
        BENCHMARK_SUSPEND
        {
            std::time_t now = std::time(0);
            boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
            boost::random::uniform_int_distribution<> randomBlock(
                1, 1024 * 1024);
            randomBlockNumber = randomBlock(gen);
        }

        fileLocation.putBlock(
            randomBlockNumber, blockSize, FileBlock{" ", " "});
    }
    folly::doNotOptimizeAway(fileLocation);
    folly::doNotOptimizeAway(randomBlockNumber);
}

BENCHMARK(benchmarkFileBlockCreation)
{
    auto fileBlock = FileBlock{" ", " "};
    folly::doNotOptimizeAway(fileBlock);
}

BENCHMARK(benchmarkToString)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    }
    fileLocation.toString();
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkProgressString)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    }
    fileLocation.progressString(blockSize, 10);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkProgressStringLarge)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.progressString(blockSize, 10);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkReplicationProgress)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    }
    fileLocation.replicationProgress(blockSize);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkReplicationProgressLarge)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.replicationProgress(blockSize);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkLinearReadReadPrefetch)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    }
    fileLocation.linearReadPrefetchThresholdReached(1, 1);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkLinearReadReadPrefetchLarge)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.linearReadPrefetchThresholdReached(1, 1);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkRandomReadReadPrefetch)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        fileLocation.putBlock(0, blockSize, FileBlock{" ", " "});
    }
    fileLocation.randomReadPrefetchThresholdReached(1, 1);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkRandomReadReadPrefetchLarge)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.randomReadPrefetchThresholdReached(1, 1);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkRandomBlocksInRangeAll)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto result = fileLocation.blocksInRange(0, 1'000'000);
    folly::doNotOptimizeAway(fileLocation);
    folly::doNotOptimizeAway(result);
}

BENCHMARK(benchmarkRandomBlocksInRangeRandom)
{
    auto fileLocation = FileLocation{};
    auto intervalBegin = 0;
    BENCHMARK_SUSPEND
    {
        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(1, 1024 * 1024);
        intervalBegin = randomBlock(gen);
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto result = fileLocation.blocksInRange(intervalBegin, 1024);
    folly::doNotOptimizeAway(fileLocation);
    folly::doNotOptimizeAway(result);
}

BENCHMARK(benchmarkUpdateBlocksInRangeLarge)
{
    auto fileLocation = FileLocation{};
    auto fileLocationUpdate = FileLocation{};
    auto randomBlockNumber = 0;

    BENCHMARK_SUSPEND
    {
        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(1, 1024 * 1024);
        FOR_EACH_RANGE(i, 0, 1'000'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }

        randomBlockNumber = randomBlock(gen);

        fileLocationUpdate.putBlock(
            randomBlockNumber, 100, FileBlock{" ", " "});
    }

    fileLocation.updateInRange(
        randomBlockNumber, randomBlockNumber + 100, fileLocationUpdate);

    folly::doNotOptimizeAway(fileLocation);
}

int main() { folly::runBenchmarks(); }