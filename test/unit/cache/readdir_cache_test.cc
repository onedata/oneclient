/**
 * @file readdir_cache_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/readdirCache.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <gtest/gtest.h>

using namespace ::testing;
using namespace one;
using namespace one::client::cache;
using namespace std::literals;

class ReaddirCacheTest : public ::testing::Test {
public:
    ReaddirCacheTest() {}
};

TEST_F(ReaddirCacheTest, dirCacheEntryIsValidShouldWork)
{
    DirCacheEntry e(100ms);

    ASSERT_FALSE(e.isValid(false));
    ASSERT_FALSE(e.isValid(true));

    e.markCreated();
    e.touch();
    ASSERT_TRUE(e.isValid(true));
    ASSERT_TRUE(e.isValid(false));

    std::this_thread::sleep_for(150ms);
    ASSERT_FALSE(e.isValid(true));
    ASSERT_TRUE(e.isValid(false));

    e.touch();
    ASSERT_TRUE(e.isValid(true));
    std::this_thread::sleep_for(5 * 100ms);
    ASSERT_FALSE(e.isValid(false));

    e.invalidate();
    ASSERT_FALSE(e.isValid(true));
    ASSERT_FALSE(e.isValid(false));

    e.touch();
    ASSERT_FALSE(e.isValid(true));
    ASSERT_FALSE(e.isValid(false));
}

TEST_F(ReaddirCacheTest, dirCacheEntryUniqueShouldWork)
{
    DirCacheEntry e(2000ms);

    folly::fbvector<folly::fbstring> dirs = {
        "dir1", "dir3", "dir3", "dir2", "dir1"};

    for (auto &d : dirs)
        e.addEntry(d);

    ASSERT_EQ(e.dirEntries().size(), dirs.size());
    e.unique();
    ASSERT_EQ(e.dirEntries().size(), 3);
}
