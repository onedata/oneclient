/**
 * @file fileCache_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "fileCache.h"

#include <gtest/gtest.h>

using namespace ::testing;
using namespace one;
using namespace one::helpers;

class FileCacheTest: public ::testing::Test
{
protected:
    FileCache cache;
    std::string buff;

    FileCacheTest()
        : cache(10)
    {
    }
};

// Test selectConnection method
TEST_F(FileCacheTest, OneBlockTest)
{
    EXPECT_TRUE(cache.writeData(5, "abcd"));

    EXPECT_FALSE(cache.readData(4, 2, buff));
    EXPECT_FALSE(cache.readData(2, 2, buff));
    EXPECT_FALSE(cache.readData(10, 1, buff));

    EXPECT_EQ(4u, cache.byteSize());

    EXPECT_TRUE(cache.readData(5, 5, buff));
    EXPECT_EQ("abcd", buff);

    EXPECT_TRUE(cache.readData(6, 5, buff));
    EXPECT_EQ("bcd", buff);

    EXPECT_TRUE(cache.readData(7, 2, buff));
    EXPECT_EQ("cd", buff);

    EXPECT_TRUE(cache.readData(7, 1, buff));
    EXPECT_EQ("c", buff);

    EXPECT_TRUE(cache.readData(7, 3, buff));
    EXPECT_EQ("cd", buff);

}

TEST_F(FileCacheTest, MultiBlockNoOverlappingTest)
{
    EXPECT_TRUE(cache.writeData(4, "45678"));
    EXPECT_TRUE(cache.writeData(0, "0123"));
    EXPECT_TRUE(cache.writeData(10, "a"));
    EXPECT_TRUE(cache.writeData(12, "c"));

    EXPECT_EQ(11u, cache.byteSize());

    EXPECT_FALSE(cache.readData(13, 2, buff));
    EXPECT_FALSE(cache.readData(11, 1, buff));

    EXPECT_TRUE(cache.readData(10, 3, buff));
    EXPECT_EQ("a", buff);

    EXPECT_TRUE(cache.readData(3, 8, buff));
    EXPECT_EQ("345678", buff);

    EXPECT_TRUE(cache.readData(4, 3, buff));
    EXPECT_EQ("456", buff);

    EXPECT_TRUE(cache.readData(12, 2, buff));
    EXPECT_EQ("c", buff);

    EXPECT_TRUE(cache.readData(10, 1, buff));
    EXPECT_EQ("a", buff);

    EXPECT_TRUE(cache.readData(10, 2, buff));
    EXPECT_EQ("a", buff);

    EXPECT_TRUE(cache.readData(1, 4, buff));
    EXPECT_EQ("1234", buff);

    EXPECT_TRUE(cache.readData(3, 6, buff));
    EXPECT_EQ("345678", buff);

    EXPECT_TRUE(cache.readData(0, 9, buff));
    EXPECT_EQ("012345678", buff);

    EXPECT_TRUE(cache.readData(2, 9, buff));
    EXPECT_EQ("2345678", buff);
}

TEST_F(FileCacheTest, MultiBlockOverlappingTest)
{
    EXPECT_TRUE(cache.writeData(4, "45678"));
    EXPECT_TRUE(cache.writeData(0, "0123"));
    EXPECT_TRUE(cache.writeData(10, "a"));
    EXPECT_TRUE(cache.writeData(12, "c"));


    EXPECT_TRUE(cache.readData(3, 6, buff));
    EXPECT_EQ("345678", buff);

    EXPECT_TRUE(cache.writeData(3, "cdef"));

    EXPECT_EQ(11u, cache.byteSize());

    EXPECT_TRUE(cache.readData(2, 6, buff));
    EXPECT_EQ("2cdef7", buff);

    EXPECT_TRUE(cache.writeData(3, "3456789"));

    EXPECT_EQ(12u, cache.byteSize());

    EXPECT_TRUE(cache.readData(1, 8, buff));
    EXPECT_EQ("12345678", buff);

    EXPECT_TRUE(cache.readData(9, 1, buff));
    EXPECT_EQ("9", buff);

    EXPECT_TRUE(cache.readData(9, 3, buff));
    EXPECT_EQ("9a", buff);

    EXPECT_TRUE(cache.writeData(3, "3456789ab"));

    EXPECT_TRUE(cache.readData(9, 3, buff));
    EXPECT_EQ("9ab", buff);

    EXPECT_TRUE(cache.readData(9, 5, buff));
    EXPECT_EQ("9abc", buff);

}

TEST_F(FileCacheTest, OldestBlockSelectionTest)
{
    //EXPECT_TRUE(cache.writeData(4, "45678"));
    EXPECT_TRUE(cache.writeData(0, "0123"));
    EXPECT_TRUE(cache.writeData(12, "c"));
    EXPECT_TRUE(cache.writeData(10, "a"));

    block_ptr block;

    block = cache.removeOldestBlock();
    EXPECT_TRUE(block.get());
    EXPECT_EQ(0, block->offset);

    EXPECT_FALSE(cache.readData(3, 1, buff));

    block = cache.removeOldestBlock();
    EXPECT_TRUE(block.get());
    EXPECT_EQ(12, block->offset);

    block = cache.removeOldestBlock();
    EXPECT_TRUE(block.get());
    EXPECT_EQ(10, block->offset);
}
