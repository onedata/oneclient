/**
 * @file inode_cache_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2026 onedata.org
 * This software is released under the MIT license cited in 'LICENSE.txt'.
 */

#include "cache/inodeCache.h"

#include <gtest/gtest.h>
#include <stdexcept>

using one::client::cache::InodeCache;

TEST(InodeCacheTest, directoryViewsHaveDistinctStableNodes)
{
    InodeCache cache{"root"};
    const auto normal = cache.generateInode("directory", "provider");
    const auto alias =
        cache.generateVirtualInode("directory", "provider", FUSE_ROOT_ID);
    const auto child =
        cache.generateVirtualInode("directory", "provider", alias);
    EXPECT_NE(normal, alias);
    EXPECT_NE(alias, child);
    EXPECT_EQ(alias,
        cache.generateVirtualInode("directory", "provider", FUSE_ROOT_ID));
    EXPECT_EQ(cache.at(normal), cache.at(alias));
    EXPECT_FALSE(cache.isVirtualInode(normal));
    EXPECT_TRUE(cache.isVirtualInode(child));
}

TEST(InodeCacheTest, renameRetiresViewsWithoutInvalidatingExistingHandles)
{
    InodeCache cache{"root"};
    const auto normal = cache.generateInode("directory", "provider");
    const auto alias =
        cache.generateVirtualInode("directory", "provider", FUSE_ROOT_ID);
    cache.rename("directory", "directory", "destination");
    const auto replacement =
        cache.generateVirtualInode("directory", "provider", FUSE_ROOT_ID);
    EXPECT_NE(alias, replacement);
    EXPECT_EQ(cache.at(alias), cache.at(replacement));
    EXPECT_EQ(normal, cache.lookup("directory"));
    cache.forget(alias, 1);
    EXPECT_THROW(cache.at(alias), std::out_of_range);
    EXPECT_EQ("directory", cache.at(replacement).first);
}

TEST(InodeCacheTest, changedUuidAndDeletionApplyToEveryView)
{
    InodeCache cache{"root"};
    const auto normal = cache.generateInode("old", "provider");
    const auto alias =
        cache.generateVirtualInode("old", "provider", FUSE_ROOT_ID);
    const auto child = cache.generateVirtualInode("old", "provider", alias);
    cache.rename("old", "new", "destination");
    EXPECT_EQ("new", cache.at(normal).first);
    EXPECT_EQ("new", cache.at(alias).first);
    EXPECT_EQ("new", cache.at(child).first);
    cache.markDeleted("new");
    for (auto node : {normal, alias, child}) {
        cache.forget(node, 1);
        EXPECT_THROW(cache.at(node), std::out_of_range);
    }
}

TEST(InodeCacheTest, virtualLookupReferencesAreBalanced)
{
    InodeCache cache{"root"};
    const auto alias =
        cache.generateVirtualInode("dir", "provider", FUSE_ROOT_ID);
    EXPECT_EQ(
        alias, cache.generateVirtualInode("dir", "provider", FUSE_ROOT_ID));
    cache.forget(alias, 1);
    EXPECT_EQ("dir", cache.at(alias).first);
    cache.forget(alias, 1);
    EXPECT_THROW(cache.at(alias), std::out_of_range);
    EXPECT_NE(
        alias, cache.generateVirtualInode("dir", "provider", FUSE_ROOT_ID));
}

TEST(InodeCacheTest, renameToCachedUuidPreservesBothHandles)
{
    InodeCache cache{"root"};
    const auto oldNode = cache.generateInode("old", "provider");
    const auto newNode = cache.generateInode("new", "provider");
    cache.rename("old", "new", "destination");
    EXPECT_EQ("new", cache.at(oldNode).first);
    EXPECT_EQ("new", cache.at(newNode).first);
    EXPECT_EQ(newNode, cache.lookup("new"));
    cache.rename("new", "again", "destination");
    EXPECT_EQ("again", cache.at(oldNode).first);
    EXPECT_EQ("again", cache.at(newNode).first);
    EXPECT_EQ(newNode, cache.lookup("again"));
}
