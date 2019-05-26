/**
 * @file protobuf_benchmark.cc
 * @author Bartek Kryza
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

std::string prepareFileLocation(const int blocksCount)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto fuseResponse = serverMsg->mutable_fuse_response();
    fuseResponse->mutable_status()->set_code(one::clproto::Status_Code_ok);
    auto fl = fuseResponse->mutable_file_location();
    fl->set_uuid("FileUuid1");
    fl->set_file_id("File1");
    fl->set_provider_id("Provider1");
    fl->set_space_id("Space1");
    fl->set_storage_id("Storage1");
    fl->set_version(1);

    for (auto i : boost::irange(0, blocksCount)) {
        auto newBlock = fl->add_blocks();
        newBlock->set_file_id("File1");
        newBlock->set_storage_id("Storage1");
        newBlock->set_offset(i * 2);
        newBlock->set_size(blockSize);
    }

    return serverMsg->SerializeAsString();
}

BENCHMARK(benchmarkDeserializeFileLocation1KBlocks)
{
    std::string serverMessage;

    BENCHMARK_SUSPEND { serverMessage = prepareFileLocation(1000); }

    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto success = serverMsg->ParseFromString(serverMessage);

    folly::doNotOptimizeAway(success);
}

BENCHMARK(benchmarkDeserializeFileLocation100KBlocks)
{
    std::string serverMessage;

    BENCHMARK_SUSPEND { serverMessage = prepareFileLocation(100'000); }

    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto success = serverMsg->ParseFromString(serverMessage);

    folly::doNotOptimizeAway(success);
}

BENCHMARK(benchmarkDeserializeFileLocation1MBlocks)
{
    std::string serverMessage;

    BENCHMARK_SUSPEND { serverMessage = prepareFileLocation(1'000'000); }

    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto success = serverMsg->ParseFromString(serverMessage);

    folly::doNotOptimizeAway(success);
}

int main() { folly::runBenchmarks(); }
