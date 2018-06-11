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

BENCHMARK_DRAW_LINE();

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

BENCHMARK_DRAW_LINE();

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
    auto res = fileLocation.toString();
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

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
    auto res = fileLocation.progressString(2 * blockSize * 1000, 50);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.progressString(2 * blockSize * 10'000, 50);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.progressString(2 * blockSize * 100'000, 50);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

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
    auto res = fileLocation.replicationProgress(2 * blockSize * 1000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.replicationProgress(2 * blockSize * 10'000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.replicationProgress(2 * blockSize * 100'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

BENCHMARK(benchmarkBlocksInRange1KBlocks)
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

BENCHMARK(benchmarkBlocksInRange10KBlocks)
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

BENCHMARK(benchmarkBlocksInRange100KBlocks)
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

BENCHMARK(benchmarkRandomBlocksCountAll100KBlocks)
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
    auto res = fileLocation.blocksInRange(0, 2 * blockSize * 100'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

BENCHMARK(benchmarkBlocksLengthInRange1KBlocks)
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
    auto res =
        fileLocation.blocksLengthInRange(blockSize * 1000, blockSize * 1100);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkBlocksLengthInRange10KBlocks)
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
    auto res = fileLocation.blocksLengthInRange(
        blockSize * 10'000, blockSize * 11'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkBlocksLengthInRange100KBlocks)
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
    auto res = fileLocation.blocksLengthInRange(
        blockSize * 100'000, blockSize * 110'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK(benchmarkRandomBlocksLengthInRangeAll100KBlocks)
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
    auto res = fileLocation.blocksLengthInRange(0, 2 * blockSize * 100'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

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
    auto res = fileLocation.linearReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 1000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.linearReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.linearReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 100'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

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
    auto res = fileLocation.randomReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 1000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.randomReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(res);
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
    auto res = fileLocation.randomReadPrefetchThresholdReached(
        0.5, 2 * blockSize * 10'000);
    folly::doNotOptimizeAway(res);
}

BENCHMARK_DRAW_LINE();

BENCHMARK(benchmarkUpdateBlocksInRange1KBlocks)
{
    auto fileLocation = FileLocation{};
    auto fileLocationUpdate = FileLocation{};
    auto randomBlockNumber = 0;

    BENCHMARK_SUSPEND
    {
        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(
            1, 1000 * blockSize);
        FOR_EACH_RANGE(i, 0, 1000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }

        randomBlockNumber = randomBlock(gen);

        fileLocationUpdate.putBlock(
            randomBlockNumber, 100 * blockSize, FileBlock{" ", " "});
    }

    fileLocation.updateInRange(
        randomBlockNumber, 100 * blockSize, fileLocationUpdate);

    folly::doNotOptimizeAway(fileLocation);
}

BENCHMARK(benchmarkUpdateBlocksInRange10KBlocks)
{
    auto fileLocation = FileLocation{};
    auto fileLocationUpdate = FileLocation{};
    auto randomBlockNumber = 0;

    BENCHMARK_SUSPEND
    {
        std::time_t now = std::time(0);
        boost::random::mt19937 gen{static_cast<std::uint32_t>(now)};
        boost::random::uniform_int_distribution<> randomBlock(
            1, 10'000 * blockSize);
        FOR_EACH_RANGE(i, 0, 10'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }

        randomBlockNumber = randomBlock(gen);

        fileLocationUpdate.putBlock(
            randomBlockNumber, 100 * blockSize, FileBlock{" ", " "});
    }

    fileLocation.updateInRange(
        randomBlockNumber, 100 * blockSize, fileLocationUpdate);

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
        boost::random::uniform_int_distribution<> randomBlock(
            1, 100'000 * blockSize);
        FOR_EACH_RANGE(i, 0, 100'000)
        {
            fileLocation.putBlock(
                2 * blockSize * i, blockSize, FileBlock{" ", " "});
        }

        randomBlockNumber = randomBlock(gen);

        fileLocationUpdate.putBlock(
            randomBlockNumber, 100 * blockSize, FileBlock{" ", " "});
    }

    fileLocation.updateInRange(
        randomBlockNumber, 100 * blockSize, fileLocationUpdate);

    folly::doNotOptimizeAway(fileLocation);
}

int main() { folly::runBenchmarks(); }
