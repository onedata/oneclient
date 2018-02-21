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
    DirCacheEntry e;

    ASSERT_FALSE(e.isValid(0s));

    e.touch();
    ASSERT_TRUE(e.isValid(1s));

    std::this_thread::sleep_for(1s);
    ASSERT_FALSE(e.isValid(500ms));

    e.touch();
    ASSERT_TRUE(e.isValid(100ms));

    e.invalidate();
    ASSERT_FALSE(e.isValid(1s));

    e.touch();
    ASSERT_FALSE(e.isValid(1s));
}

TEST_F(ReaddirCacheTest, dirCacheEntryUniqueShouldWork)
{
    DirCacheEntry e;

    folly::fbvector<folly::fbstring> dirs = {
        "dir1", "dir3", "dir3", "dir2", "dir1"};

    for (auto &d : dirs)
        e.addEntry(d);

    ASSERT_EQ(e.dirEntries().size(), dirs.size());
    e.unique();
    ASSERT_EQ(e.dirEntries().size(), 3);
}
