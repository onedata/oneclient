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

BENCHMARK(benchmarkPut1KBlocks)
{
    auto fileLocation = FileLocation{};
    FOR_EACH_RANGE(i, 0, 1000)
    {
        fileLocation.putBlock(
            2 * blockSize * i, blockSize, FileBlock{" ", " "});
    }
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPut10KBlocks)
{
    auto fileLocation = FileLocation{};
    FOR_EACH_RANGE(i, 0, 10'000)
    {
        fileLocation.putBlock(
            2 * blockSize * i, blockSize, FileBlock{" ", " "});
    }
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPut100KBlocks)
{
    auto fileLocation = FileLocation{};
    FOR_EACH_RANGE(i, 0, 100'000)
    {
        fileLocation.putBlock(
            2 * blockSize * i, blockSize, FileBlock{" ", " "});
    }
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkPutBlockRandomlyAfter100KBlocks)
{
    auto fileLocation = FileLocation{};
    auto randomBlockNumber = 0;
    BENCHMARK_SUSPEND
    {
        for (auto i : boost::irange(1, 100'000))
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

BENCHMARK(benchmarkProgressString1KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.progressString(2 * blockSize * 1000, 50);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkProgressString10KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.progressString(2 * blockSize * 10'000, 50);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkProgressString100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.progressString(2 * blockSize * 100'000, 50);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkReplicationProgress1KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.replicationProgress(2 * blockSize * 1000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkReplicationProgress10KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.replicationProgress(2 * blockSize * 10'000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkReplicationProgress100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.replicationProgress(2 * blockSize * 100'000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkBlocksCount1KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto res = fileLocation.blocksInRange(blockSize * 1000, blockSize * 1100);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkBlocksCount10KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto res =
        fileLocation.blocksInRange(blockSize * 10'000, blockSize * 11'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkBlocksCount100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto res =
        fileLocation.blocksInRange(blockSize * 100'000, blockSize * 110'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkRandomBlocksInRangeAll100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    auto result = fileLocation.blocksInRange(0, 100'000);
    folly::doNotOptimizeAway(fileLocation);
    folly::doNotOptimizeAway(result);
}

BENCHMARK(benchmarkLinearReadReadPrefetch1KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.linearReadPrefetchThresholdReached(0.5, 2 * blockSize * 1000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkLinearReadReadPrefetch10KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.linearReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkLinearReadReadPrefetch100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.linearReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 100'000);
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

BENCHMARK(benchmarkRandomReadReadPrefetch1KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.randomReadPrefetchThresholdReached(0.5, 2 * blockSize * 1000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkRandomReadReadPrefetch10KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.randomReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkRandomReadReadPrefetch100KBlocks)
{
    auto fileLocation = FileLocation{};
    BENCHMARK_SUSPEND
    {
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }
    }
    fileLocation.randomReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkUpdateBlocksInRange100KBlocks)
{
    auto fileLocation = FileLocation{};
    auto fileLocationUpdate = FileLocation{};
    auto randomBlockNumber = 0;

    BENCHMARK_SUSPEND
    {
        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(1, 1024 * 1024);
        FOR_EACH_RANGE(i, 0, 100'000)
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
