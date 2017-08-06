/**
 * @file glusterfsHelper_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "glusterfsHelper.h"
#include "testUtils.h"

#include <boost/make_shared.hpp>
#include <gtest/gtest.h>

#include <tuple>

using namespace ::testing;
using namespace one;
using namespace one::helpers;
using namespace one::testing;

struct GlusterFSHelperTest : public ::testing::Test {
    GlusterFSHelperTest() {}

    ~GlusterFSHelperTest() {}

    void SetUp() override {}

    void TearDown() override {}
};

TEST_F(
    GlusterFSHelperTest, relativePathsShouldBeProperlyAliasedWithinMountpoint)
{
    using namespace boost::filesystem;

    EXPECT_EQ("file1.txt", makeRelative("/DIR1/DIR2", "/DIR1/DIR2/file1.txt"));
    EXPECT_EQ("file1.txt", makeRelative("/DIR1/DIR2", "DIR1/DIR2/file1.txt"));
    EXPECT_EQ("file1.txt", makeRelative("DIR1/DIR2", "/DIR1/DIR2/file1.txt"));
    EXPECT_EQ("file1.txt", makeRelative("DIR1/DIR2", "DIR1/DIR2/file1.txt"));
    EXPECT_EQ("DIR1/DIR2/file1.txt",
        makeRelative("DIR2/DIR1", "DIR1/DIR2/file1.txt"));
    EXPECT_EQ("DIR1/DIR2/file1.txt", makeRelative("/", "DIR1/DIR2/file1.txt"));
    EXPECT_EQ(
        "file1.txt", makeRelative("/DIR1/DIR2", "DIR1/DIR2/DIR3/../file1.txt"));
    EXPECT_EQ(
        "file1.txt", makeRelative("/DIR1/DIR2", "DIR1/DIR2/../../file1.txt"));
    EXPECT_EQ("file1.txt",
        makeRelative(
            "/DIR1/DIR2", "DIR1/DIR2/../../../DIR3/../../../file1.txt"));

    auto helper = std::make_unique<GlusterFSHelper>(
        "/DIR1/DIR2", 0, 0, "", 0, "", "", "", nullptr);

    EXPECT_EQ("file1.txt", helper->relative("DIR1/DIR2/file1.txt"));
    EXPECT_EQ("file1.txt", helper->relative("/DIR1/DIR2/file1.txt"));

    auto helper2 =
        std::make_unique<GlusterFSHelper>("", 0, 0, "", 0, "", "", "", nullptr);

    EXPECT_EQ("DIR1/DIR2/file1.txt", helper2->relative("DIR1/DIR2/file1.txt"));
    EXPECT_EQ("DIR1/DIR2/file1.txt", helper2->relative("/DIR1/DIR2/file1.txt"));

    auto helper3 = std::make_unique<GlusterFSHelper>(
        "/", 0, 0, "", 0, "", "", "", nullptr);

    EXPECT_EQ("DIR1/DIR2/file1.txt", helper2->relative("DIR1/DIR2/file1.txt"));
    EXPECT_EQ("DIR1/DIR2/file1.txt", helper2->relative("/DIR1/DIR2/file1.txt"));
}

TEST_F(GlusterFSHelperTest, xlatorOptionsParsingShouldWork)
{
    auto options = GlusterFSHelper::parseXlatorOptions("");
    EXPECT_EQ(options.size(), 0);

    options.clear();
    options =
        GlusterFSHelper::parseXlatorOptions("cluster.write-freq-threshold=100");
    EXPECT_EQ(options.size(), 1);
    EXPECT_EQ(options[0].first, "cluster.write-freq-threshold");
    EXPECT_EQ(options[0].second, "100");

    options.clear();
    options = GlusterFSHelper::parseXlatorOptions(
        "cluster.write-freq-threshold=100;");
    EXPECT_EQ(options.size(), 1);
    EXPECT_EQ(options[0].first, "cluster.write-freq-threshold");
    EXPECT_EQ(options[0].second, "100");

    options.clear();
    options = GlusterFSHelper::parseXlatorOptions(
        ";cluster.write-freq-threshold=100");
    EXPECT_EQ(options.size(), 1);
    EXPECT_EQ(options[0].first, "cluster.write-freq-threshold");
    EXPECT_EQ(options[0].second, "100");

    options.clear();
    options = GlusterFSHelper::parseXlatorOptions(
        ";cluster.write-freq-threshold=100;;;");
    EXPECT_EQ(options.size(), 1);
    EXPECT_EQ(options[0].first, "cluster.write-freq-threshold");
    EXPECT_EQ(options[0].second, "100");

    options.clear();
    options = GlusterFSHelper::parseXlatorOptions(
        "cluster.write-freq-threshold=100;features.record-counters=20;"
        "performance.read-ahead-page-count=1000");
    EXPECT_EQ(options.size(), 3);
    EXPECT_EQ(options[0].first, "cluster.write-freq-threshold");
    EXPECT_EQ(options[0].second, "100");
    EXPECT_EQ(options[1].first, "features.record-counters");
    EXPECT_EQ(options[1].second, "20");
    EXPECT_EQ(options[2].first, "performance.read-ahead-page-count");
    EXPECT_EQ(options[2].second, "1000");

    ASSERT_THROW(
        GlusterFSHelper::parseXlatorOptions("write-freq-threshold==100"),
        std::runtime_error);

    ASSERT_THROW(
        GlusterFSHelper::parseXlatorOptions("cluster.write-freq-threshold="),
        std::runtime_error);

    ASSERT_THROW(
        GlusterFSHelper::parseXlatorOptions("cluster=write-freq-threshold=100"),
        std::runtime_error);

    ASSERT_THROW(
        GlusterFSHelper::parseXlatorOptions("cluster.write-freq-threshold"),
        std::runtime_error);
}