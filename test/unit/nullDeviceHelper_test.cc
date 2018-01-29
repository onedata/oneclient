/**
 * @file nullDeviceHelper_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "nullDeviceHelper.h"
#include "testUtils.h"

#include <boost/make_shared.hpp>
#include <folly/futures/ManualExecutor.h>
#include <gtest/gtest.h>

#include <tuple>

using namespace ::testing;
using namespace one;
using namespace one::helpers;
using namespace one::testing;

struct NullDeviceHelperTest : public ::testing::Test {
    NullDeviceHelperTest() {}

    ~NullDeviceHelperTest() {}

    void SetUp() override {}

    void TearDown() override {}

    std::shared_ptr<folly::ManualExecutor> m_executor =
        std::make_shared<folly::ManualExecutor>();
};

TEST_F(NullDeviceHelperTest, timeoutWithZeroProbabilityShouldAlwaysBeFalse)
{
    NullDeviceHelper helper(
        0, 0, 0.0, "*", std::make_shared<folly::ManualExecutor>());

    for (int i = 0; i < 1000; i++)
        EXPECT_FALSE(helper.randomTimeout());
}

TEST_F(NullDeviceHelperTest, timeoutWithOneProbabilityShouldAlwaysBeTrue)
{
    NullDeviceHelper helper(
        0, 0, 1.0, "*", std::make_shared<folly::ManualExecutor>());

    for (int i = 0; i < 1000; i++)
        EXPECT_TRUE(helper.randomTimeout());
}

TEST_F(NullDeviceHelperTest, latencyShouldBeAlwaysInDefinedRange)
{
    NullDeviceHelper helper(
        250, 750, 1.0, "*", std::make_shared<folly::ManualExecutor>());

    for (int i = 0; i < 1000; i++) {
        auto latency = helper.randomLatency();
        EXPECT_TRUE(latency > 100);
        EXPECT_TRUE(latency < 1000);
    }
}

TEST_F(NullDeviceHelperTest, latencyWithZeroRangeShouldBeAlwaysReturnZero)
{
    NullDeviceHelper helper(
        0, 0, 1.0, "*", std::make_shared<folly::ManualExecutor>());

    for (int i = 0; i < 1000; i++)
        EXPECT_EQ(helper.randomLatency(), 0);
}

TEST_F(NullDeviceHelperTest, emptyFilterShouldAllowAnyOperation)
{
    NullDeviceHelper helper(
        100, 1000, 1.0, "", std::make_shared<folly::ManualExecutor>());

    EXPECT_TRUE(helper.applies("whatever"));
}

TEST_F(NullDeviceHelperTest, wildcardFilterShouldAllowAnyOperation)
{
    NullDeviceHelper helper(
        100, 1000, 1.0, "*", std::make_shared<folly::ManualExecutor>());

    EXPECT_TRUE(helper.applies("whatever"));
}

TEST_F(NullDeviceHelperTest, singleWordFileterShouldAllowOnlyOneOperations)
{
    NullDeviceHelper helper(
        100, 1000, 1.0, "truncate", std::make_shared<folly::ManualExecutor>());

    EXPECT_FALSE(helper.applies("whatever"));
    EXPECT_FALSE(helper.applies(""));
    EXPECT_TRUE(helper.applies("truncate"));
}

TEST_F(NullDeviceHelperTest, multipleOpsShouldAllowOnlyTheseOperations)
{
    NullDeviceHelper helper(100, 1000, 1.0, "truncate,read,write",
        std::make_shared<folly::ManualExecutor>());

    EXPECT_FALSE(helper.applies("whatever"));
    EXPECT_FALSE(helper.applies(""));
    EXPECT_TRUE(helper.applies("truncate"));
    EXPECT_TRUE(helper.applies("read"));
    EXPECT_TRUE(helper.applies("write"));
}

TEST_F(
    NullDeviceHelperTest, multipleOpsWithSpacesShouldAllowOnlyTheseOperations)
{
    NullDeviceHelper helper(100, 1000, 1.0,
        "\t\t\ntruncate,\nread,\n   write  ",
        std::make_shared<folly::ManualExecutor>());

    EXPECT_FALSE(helper.applies("whatever"));
    EXPECT_FALSE(helper.applies(""));
    EXPECT_TRUE(helper.applies("truncate"));
    EXPECT_TRUE(helper.applies("read"));
    EXPECT_TRUE(helper.applies("write"));
}

TEST_F(NullDeviceHelperTest, readReturnsRequestedNumberOfBytes)
{
    auto helper =
        std::make_shared<NullDeviceHelper>(0, 0, 0.0, "*", m_executor);

    auto handle = helper->open("whatever", O_RDWR, {}).getVia(m_executor.get());

    EXPECT_EQ(
        handle->read(1000, 100).getVia(m_executor.get()).chainLength(), 100);
}

TEST_F(NullDeviceHelperTest, writeReturnsWrittenNumberOfBytes)
{
    auto helper =
        std::make_shared<NullDeviceHelper>(0, 0, 0.0, "*", m_executor);

    auto handle = helper->open("whatever", O_RDWR, {}).getVia(m_executor.get());

    std::size_t size = 10 * 1024 * 1024;
    std::string stmp(size, 'y');
    std::string tmp;

    folly::IOBufQueue writeBuf{folly::IOBufQueue::cacheChainLength()};
    writeBuf.append(stmp);

    auto bytesWritten =
        handle->write(0, std::move(writeBuf)).getVia(m_executor.get());

    EXPECT_EQ(bytesWritten, size);
}

TEST_F(NullDeviceHelperTest, readTimesAreInLatencyBoundaries)
{
    auto helper =
        std::make_shared<NullDeviceHelper>(25, 75, 0.0, "*", m_executor);

    auto handle = helper->open("whatever", O_RDWR, {}).getVia(m_executor.get());

    for (int i = 0; i < 50; i++) {
        auto start = std::chrono::steady_clock::now();

        handle->read(1000, 100).getVia(m_executor.get());

        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - start);

        EXPECT_TRUE(duration.count() >= 25);
        EXPECT_TRUE(duration.count() <= 75 + 10);
    }
}
