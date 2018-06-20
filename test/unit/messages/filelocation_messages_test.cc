/**
 * @file filelocation_messages_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileLocation.h"

#include <gtest/gtest.h>

using namespace one::messages::fuse;

/**
 * The purpose of this test suite is to test the file location message
 * and in particular the file blocks map rendering.
 */
struct FuseFileLocationMessagesTest : public ::testing::Test {
};

TEST_F(FuseFileLocationMessagesTest, replicationProgressShouldWork)
{
    auto fileLocation = FileLocation{};
    EXPECT_EQ(fileLocation.replicationProgress(1024), 0.0);
    fileLocation.putBlock(0, 1024, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(1024), 1.0);
}

TEST_F(FuseFileLocationMessagesTest, blocksInRangeCounterShouldWork)
{
    auto fileLocation = FileLocation{};
    EXPECT_EQ(fileLocation.blocksInRange(256, 1024), 0);

    fileLocation.putBlock(0, 512, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksInRange(1024, 1500), 0);
    EXPECT_EQ(fileLocation.blocksInRange(256, 1024), 1);

    fileLocation.putBlock(515, 5, FileBlock{"", ""});
    fileLocation.putBlock(600, 10, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksInRange(256, 530), 2);
    EXPECT_EQ(fileLocation.blocksInRange(256, 1024), 3);

    fileLocation.putBlock(1000, 200, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksInRange(515, 650), 2);
    EXPECT_EQ(fileLocation.blocksInRange(0, 650), 3);
    EXPECT_EQ(fileLocation.blocksInRange(256, 1024), 4);
}

TEST_F(FuseFileLocationMessagesTest, blocksLengthInRangeShouldWork)
{
    auto fileLocation = FileLocation{};
    EXPECT_EQ(fileLocation.blocksLengthInRange(250, 1000), 0);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 500, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksLengthInRange(0, 1000), 500);

    fileLocation.putBlock(500, 1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksInRange(0, 1000), 1);
    EXPECT_EQ(fileLocation.blocksLengthInRange(0, 1000), 501);
    EXPECT_EQ(fileLocation.blocksLengthInRange(500, 1000), 1);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 25, FileBlock{"", ""});
    fileLocation.putBlock(100, 25, FileBlock{"", ""});
    fileLocation.putBlock(200, 25, FileBlock{"", ""});
    fileLocation.putBlock(300, 25, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.blocksLengthInRange(0, 1000), 100);
    EXPECT_EQ(fileLocation.blocksLengthInRange(0, 320), 95);
}

TEST_F(FuseFileLocationMessagesTest, updateInRangeShouldAddNewBlocks)
{
    auto oldFileLocation = FileLocation{};
    oldFileLocation.putBlock(0, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(100, 10, FileBlock{"", ""});

    auto fileLocationChange = FileLocation{};
    fileLocationChange.putBlock(50, 10, FileBlock{"", ""});

    oldFileLocation.updateInRange(40, 70, fileLocationChange);

    auto targetFileLocation = FileLocation{};
    targetFileLocation.putBlock(0, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(50, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(100, 10, FileBlock{"", ""});

    EXPECT_TRUE(oldFileLocation.blocks() == targetFileLocation.blocks());
    EXPECT_EQ(oldFileLocation.toString(), targetFileLocation.toString());
}

TEST_F(FuseFileLocationMessagesTest, updateInRangeShouldRemoveNewBlocks)
{
    auto oldFileLocation = FileLocation{};
    oldFileLocation.putBlock(0, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(50, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(100, 10, FileBlock{"", ""});

    auto fileLocationChange = FileLocation{};

    oldFileLocation.updateInRange(40, 70, fileLocationChange);

    auto targetFileLocation = FileLocation{};
    targetFileLocation.putBlock(0, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(100, 10, FileBlock{"", ""});

    EXPECT_TRUE(oldFileLocation.blocks() == targetFileLocation.blocks());
    EXPECT_EQ(oldFileLocation.toString(), targetFileLocation.toString());
}

TEST_F(FuseFileLocationMessagesTest, updateInRangeShouldReplaceBlocks)
{
    auto oldFileLocation = FileLocation{};
    oldFileLocation.putBlock(0, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(50, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(65, 2, FileBlock{"", ""});
    oldFileLocation.putBlock(100, 10, FileBlock{"", ""});

    auto fileLocationChange = FileLocation{};
    fileLocationChange.putBlock(40, 25, FileBlock{"", ""});

    oldFileLocation.updateInRange(40, 70, fileLocationChange);

    auto targetFileLocation = FileLocation{};
    targetFileLocation.putBlock(0, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(40, 25, FileBlock{"", ""});
    targetFileLocation.putBlock(100, 10, FileBlock{"", ""});

    EXPECT_TRUE(oldFileLocation.blocks() == targetFileLocation.blocks());
    EXPECT_EQ(oldFileLocation.toString(), targetFileLocation.toString());
}

TEST_F(FuseFileLocationMessagesTest, updateInRangeShouldIgnoreEmptyChange)
{
    auto oldFileLocation = FileLocation{};
    oldFileLocation.putBlock(0, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(50, 10, FileBlock{"", ""});
    oldFileLocation.putBlock(65, 2, FileBlock{"", ""});
    oldFileLocation.putBlock(100, 10, FileBlock{"", ""});

    auto fileLocationChange = FileLocation{};
    fileLocationChange.putBlock(40, 25, FileBlock{"", ""});

    oldFileLocation.updateInRange(0, 0, fileLocationChange);

    auto targetFileLocation = FileLocation{};
    targetFileLocation.putBlock(0, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(50, 10, FileBlock{"", ""});
    targetFileLocation.putBlock(65, 2, FileBlock{"", ""});
    targetFileLocation.putBlock(100, 10, FileBlock{"", ""});

    EXPECT_TRUE(oldFileLocation.blocks() == targetFileLocation.blocks());
    EXPECT_EQ(oldFileLocation.toString(), targetFileLocation.toString());
}

TEST_F(FuseFileLocationMessagesTest, linearReadPrefetchThresholdReachedMustWork)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, 512, FileBlock{"", ""});
    EXPECT_TRUE(fileLocation.linearReadPrefetchThresholdReached(0.4, 1024));
    EXPECT_FALSE(fileLocation.linearReadPrefetchThresholdReached(0.6, 1024));
}

TEST_F(FuseFileLocationMessagesTest, randomReadPrefetchThresholdReachedMustWork)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, 50, FileBlock{"", ""});
    fileLocation.putBlock(100, 50, FileBlock{"", ""});
    fileLocation.putBlock(500, 50, FileBlock{"", ""});
    fileLocation.putBlock(600, 50, FileBlock{"", ""});
    EXPECT_FALSE(fileLocation.randomReadPrefetchThresholdReached(0.2, 1024));
    fileLocation.putBlock(700, 50, FileBlock{"", ""});
    fileLocation.putBlock(800, 50, FileBlock{"", ""});
    EXPECT_TRUE(fileLocation.randomReadPrefetchThresholdReached(0.2, 1024));
}

TEST_F(FuseFileLocationMessagesTest, emptyFileLocationShouldRenderEmptyProgress)
{
    auto fileLocation = FileLocation{};

    std::string expected(10, ' ');

    EXPECT_EQ(fileLocation.progressString(1024, 10), expected);
    EXPECT_EQ(fileLocation.progressString(10, 10), expected);
    EXPECT_EQ(fileLocation.progressString(5, 10), expected);
    EXPECT_EQ(fileLocation.progressString(1, 10), expected);
    EXPECT_EQ(fileLocation.progressString(0, 10), expected);
}

TEST_F(FuseFileLocationMessagesTest,
    completeFileLocationShouldRenderCompleteProgress)
{
    std::string expected(10, '#');

    auto fileLocationLarge = FileLocation{};
    fileLocationLarge.putBlock(0, 1024, FileBlock{"", ""});
    EXPECT_EQ(fileLocationLarge.progressString(1024, 10), expected);

    auto fileLocationEqual = FileLocation{};
    fileLocationEqual.putBlock(0, 10, FileBlock{"", ""});
    EXPECT_EQ(fileLocationEqual.progressString(10, 10), expected);

    auto fileLocationSmall = FileLocation{};
    fileLocationSmall.putBlock(0, 2, FileBlock{"", ""});
    EXPECT_EQ(fileLocationSmall.progressString(2, 10), expected);
}

TEST_F(FuseFileLocationMessagesTest,
    partialFileLocationShouldRenderPartialProgress)
{
    auto fileLocation = FileLocation{};
    const auto fileSize = 1000;
    fileLocation.putBlock(0, 1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), ".         ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize * 0.1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "#         ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize * 0.9, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "######### ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize / 2, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "#####     ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize / 2 + 1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "#####.    ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize / 2 - 25, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "####o     ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, fileSize / 2 + 75, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "#####o    ");

    fileLocation = FileLocation{};
    fileLocation.putBlock(fileSize / 2, fileSize / 2, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "     #####");

    fileLocation = FileLocation{};
    fileLocation.putBlock(
        fileSize * 0.93, fileSize - fileSize * 0.93, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(fileSize, 10), "         o");
}

TEST_F(FuseFileLocationMessagesTest,
    partialFileLocationShouldRenderPartialProgressForSmallFiles)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, 1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(15, 10), "..........");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 10, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(15, 10), "oooooooooo");

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 15, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.progressString(15, 10), "##########");
}

TEST_F(
    FuseFileLocationMessagesTest, replicationProgressShouldReportProperValues)
{
    auto fileLocation = FileLocation{};
    fileLocation.putBlock(0, 1, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(100), 0.01);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 512, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(1024), 0.5);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 100, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(0), 0);

    fileLocation = FileLocation{};
    EXPECT_EQ(fileLocation.replicationProgress(1024), 0);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 100, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(100), 1.0);

    fileLocation = FileLocation{};
    fileLocation.putBlock(0, 100, FileBlock{"", ""});
    EXPECT_EQ(fileLocation.replicationProgress(50), 1.0);
}
