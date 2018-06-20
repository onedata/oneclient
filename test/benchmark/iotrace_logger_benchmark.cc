/**
 * @file iotrace_logger_benchmark.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fslogic/ioTraceLogger.h"

#include <boost/filesystem.hpp>
#include <boost/random.hpp>
#include <boost/range/irange.hpp>
#include <folly/Benchmark.h>
#include <folly/Foreach.h>

#include <iostream>
#include <utility>

using namespace one::client::fslogic;

BENCHMARK(benchmarkLog1MRecordsFlushEvery1)
{
    auto tempFile = boost::filesystem::temp_directory_path() /
        boost::filesystem::unique_path();
    auto tracer = IOTraceLogger::make(tempFile.native(), 1);

    for (auto i : boost::irange(0, 1'000'000)) {
        tracer->log(std::chrono::system_clock::now(),
            IOTraceLogger::OpType::READ, std::chrono::microseconds{1000},
            i + 1024, 1024, false, 2048);
    };

    tracer->stop();

    boost::filesystem::remove(tempFile);

    folly::doNotOptimizeAway(tracer);
}

BENCHMARK(benchmarkLog1MRecordsFlushEvery1K)
{
    auto tempFile = boost::filesystem::temp_directory_path() /
        boost::filesystem::unique_path();
    auto tracer = IOTraceLogger::make(tempFile.native(), 1'000);

    for (auto i : boost::irange(0, 1'000'000)) {
        tracer->log(std::chrono::system_clock::now(),
            IOTraceLogger::OpType::READ, std::chrono::microseconds{1000},
            i + 1024, 1024, false, 2048);
    };

    tracer->stop();

    boost::filesystem::remove(tempFile);

    folly::doNotOptimizeAway(tracer);
}

BENCHMARK(benchmarkLog1MRecordsFlushEvery10K)
{
    auto tempFile = boost::filesystem::temp_directory_path() /
        boost::filesystem::unique_path();
    auto tracer = IOTraceLogger::make(tempFile.native(), 10'000);

    for (auto i : boost::irange(0, 1'000'000)) {
        tracer->log(std::chrono::system_clock::now(),
            IOTraceLogger::OpType::READ, std::chrono::microseconds{1000},
            i + 1024, 1024, false, 2048);
    };

    tracer->stop();

    boost::filesystem::remove(tempFile);

    folly::doNotOptimizeAway(tracer);
}

BENCHMARK(benchmarkLog1MRecordsFlushEvery100K)
{
    auto tempFile = boost::filesystem::temp_directory_path() /
        boost::filesystem::unique_path();
    auto tracer = IOTraceLogger::make(tempFile.native(), 100'000);

    for (auto i : boost::irange(0, 1'000'000)) {
        tracer->log(std::chrono::system_clock::now(),
            IOTraceLogger::OpType::READ, std::chrono::microseconds{1000},
            i + 1024, 1024, false, 2048);
    };

    tracer->stop();

    boost::filesystem::remove(tempFile);

    folly::doNotOptimizeAway(tracer);
}

int main() { folly::runBenchmarks(); }
