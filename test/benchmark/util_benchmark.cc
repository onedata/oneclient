/**
 * @file util_benchmark.cc
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "util/base64.h"
#include "util/cdmi.h"
#include "util/uuid.h"

#include <boost/random.hpp>
#include <boost/range/irange.hpp>
#include <folly/Benchmark.h>
#include <folly/Foreach.h>

#include <utility>

using namespace one::client::util;

BENCHMARK(benchmarkUuidToSpaceId)
{
    auto spaceId = uuid::uuidToSpaceId(
        "Z3VpZCMwZjUwMzQxZWM4YTZjYTUyYjQ4ZmUxMWExMTQ4NWI4MGNoZDRjZiM3ZDZlZGFlYT"
        "RjNTNiNmE5ZDAxNTZjNjU3NjRmMTk0ZWNoZDRjZg");
    folly::doNotOptimizeAway(spaceId);
}

BENCHMARK(benchmarkSpaceIdToSpaceUUID)
{
    auto spaceUUID =
        uuid::spaceIdToSpaceUUID("7d6edaea4c53b6a9d0156c65764f194echd4cf");
    folly::doNotOptimizeAway(spaceUUID);
}

BENCHMARK_DRAW_LINE();

BENCHMARK(benchmarkUUIDToObjectId)
{
    auto objectId = cdmi::uuidToObjectId(
        "Z3VpZCMwZjUwMzQxZWM4YTZjYTUyYjQ4ZmUxMWExMTQ4NWI4MGNoZDRjZiM3ZDZlZGFlYT"
        "RjNTNiNmE5ZDAxNTZjNjU3NjRmMTk0ZWNoZDRjZg");
    folly::doNotOptimizeAway(objectId);
}

int main() { folly::runBenchmarks(); }
