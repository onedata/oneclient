/**
 * @file cache_expiration_helper_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/cacheExpirationHelper.h"

#include "testUtils.h"

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <functional>

using namespace ::testing;
using namespace std::literals;

struct CacheExpirationHelperTest : public ::testing::Test {
    int key = one::testing::randomInt();
    int key1 = key + 1;
    int key2 = key + 2;

    bool purgeCalled = false;
    bool purgeCalledK1 = false;
    bool purgeCalledK2 = false;
    bool cacheCalled = false;

    std::function<void(int)> purge = [this](int k) {
        purgeCalled |= k == key;
        purgeCalledK1 |= k == key1;
        purgeCalledK2 |= k == key2;
    };

    std::function<void()> cache = [this] { cacheCalled = true; };
};

TEST_F(CacheExpirationHelperTest, shouldExpireAnEntryAfterCountCallsToTick)
{
    one::client::CacheExpirationHelper<int, 13> expirationHelper;

    expirationHelper.markInteresting(key, [] {});

    for (int i = 0; i < 12; ++i) {
        expirationHelper.tick(purge);
        ASSERT_FALSE(purgeCalled);
    }

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldRenewTimerAfterMarkInteresting)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.markInteresting(key1, [] {});
    expirationHelper.markInteresting(key2, [] {});

    // Both keys have 2 ticks left
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalledK1);
    ASSERT_FALSE(purgeCalledK2);

    expirationHelper.markInteresting(key1, [] {});

    // K1 has 2 ticks left, K2 1 tick
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalledK1);
    ASSERT_TRUE(purgeCalledK2);

    // K1 has 1 tick left
    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalledK1);
    ASSERT_TRUE(purgeCalledK2);
}

TEST_F(CacheExpirationHelperTest, shouldCallPurgeOnlyOnceIfKeyWasntReintroduced)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.markInteresting(key, [] {});

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);

    purgeCalled = false;
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldCallPurgeForReintroducedKey)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.markInteresting(key, [] {});

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);

    expirationHelper.markInteresting(key, [] {});

    purgeCalled = false;
    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldNotPurgePinnedEntry)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldPurgeUnpinnedEntry)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.tick(purge);
    expirationHelper.unpin(key);
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldNotPurgeInterestingThenPinnedEntry)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.markInteresting(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldNotPurgePinnedThenInterestingEntry)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.markInteresting(key, [] {});
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldPurgeInterestingThenPinnedUnpinnedEntry)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.markInteresting(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.unpin(key);
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldPurgePinnedThenInterestingUnpinnedEntry)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.pin(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.markInteresting(key, [] {});
    expirationHelper.tick(purge);
    expirationHelper.tick(purge);
    expirationHelper.unpin(key);
    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldCallCacheFunctionOnFirstPinAddition)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.pin(key, cache);
    ASSERT_TRUE(cacheCalled);

    cacheCalled = false;
    expirationHelper.pin(key, cache);
    ASSERT_FALSE(cacheCalled);

    expirationHelper.markInteresting(key, cache);
    ASSERT_FALSE(cacheCalled);
}

TEST_F(CacheExpirationHelperTest, shouldCallCacheFunctionOnFirstMarkAddition)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.markInteresting(key, cache);
    ASSERT_TRUE(cacheCalled);

    cacheCalled = false;
    expirationHelper.pin(key, cache);
    ASSERT_FALSE(cacheCalled);

    expirationHelper.markInteresting(key, cache);
    ASSERT_FALSE(cacheCalled);
}

TEST_F(CacheExpirationHelperTest, shouldCallCacheFunctionOnReaddition)
{
    one::client::CacheExpirationHelper<int, 1> expirationHelper;

    expirationHelper.markInteresting(key, cache);
    ASSERT_TRUE(cacheCalled);

    expirationHelper.tick(purge);

    cacheCalled = false;
    expirationHelper.markInteresting(key, cache);
    ASSERT_TRUE(cacheCalled);
}

TEST_F(CacheExpirationHelperTest, shouldExpireAnEntryOnExpire)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.markInteresting(key, [] {});
    expirationHelper.expire(key);
    expirationHelper.tick(purge);

    ASSERT_TRUE(purgeCalled);
}

TEST_F(CacheExpirationHelperTest, shouldAllowRescuingAnEntryAfterExpire)
{
    one::client::CacheExpirationHelper<int, 2> expirationHelper;

    expirationHelper.markInteresting(key, [] {});
    expirationHelper.expire(key);
    expirationHelper.markInteresting(key, [] {});

    expirationHelper.tick(purge);
    ASSERT_FALSE(purgeCalled);

    expirationHelper.tick(purge);
    ASSERT_TRUE(purgeCalled);
}
