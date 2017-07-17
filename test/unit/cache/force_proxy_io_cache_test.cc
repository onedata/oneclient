/**
 * @file force_proxy_io_cache_test.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/forceProxyIOCache.h"

#include <gtest/gtest.h>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;

class ForceProxyIOCacheTest : public ::testing::Test {
public:
    ForceProxyIOCacheTest()
    {
        forceProxyIOCache.onAdd([this](auto) { ++onAddCounter; });
        forceProxyIOCache.onRemove([this](auto) { ++onRemoveCounter; });
    }

protected:
    int onAddCounter = 0;
    int onRemoveCounter = 0;
    cache::ForceProxyIOCache forceProxyIOCache;
};

TEST_F(ForceProxyIOCacheTest, containsShouldReturnFalseIfFileNotCached)
{
    folly::fbstring uuid = "uuid";
    EXPECT_FALSE(forceProxyIOCache.contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, addShouldAddFile)
{
    folly::fbstring uuid = "uuid";
    forceProxyIOCache.add(uuid);
    ASSERT_TRUE(forceProxyIOCache.contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, addShouldCallCallback)
{
    folly::fbstring uuid = "uuid";
    forceProxyIOCache.add(uuid);
    ASSERT_EQ(1, onAddCounter);
}

TEST_F(ForceProxyIOCacheTest, removeShouldRemoveFile)
{
    folly::fbstring uuid = "uuid";
    forceProxyIOCache.add(uuid);
    forceProxyIOCache.remove(uuid);

    ASSERT_FALSE(forceProxyIOCache.contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, removeShouldCallCallback)
{
    folly::fbstring uuid = "uuid";
    forceProxyIOCache.add(uuid);
    forceProxyIOCache.remove(uuid);
    ASSERT_EQ(1, onRemoveCounter);
}
