/**
 * @file directIOHelper_test.cc
 * @author Rafal Slota
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "directIOHelper.h"

#include "testUtils.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <folly/futures/ManualExecutor.h>
#include <gtest/gtest.h>

#include <errno.h>

#include <cstdio>
#include <cstring>
#include <fstream>
#include <functional>
#include <future>
#include <iostream>

using namespace ::testing;
using namespace one;
using namespace one::helpers;
using namespace one::testing;
using namespace std::placeholders;
using namespace std::literals;

template <typename T> bool identityEqual(const T &lhs, const T &rhs)
{
    return &lhs == &rhs;
}

constexpr int BENCH_BLOCK_SIZE = 1024 * 100;
constexpr int BENCH_LOOP_COUNT = 10000;

const auto TEST_ROOT =
    boost::filesystem::temp_directory_path() / "directIOHelper_test";

struct DirectIOHelperTest : public ::testing::Test {
    DirectIOHelperTest()
    {
        boost::filesystem::create_directories(root);

        // remove all files that are used in tests
        unlinkOnDIO("to");
        unlinkOnDIO("dir");
        unlinkOnDIO(testFileId);

        // create test file
        std::ofstream f(testFilePath.string());
        f << "test_123456789_test" << std::endl;
        f.close();

        executor = std::make_shared<folly::ManualExecutor>();

        proxy = std::make_shared<DirectIOHelper>(
            root, getuid(), getgid(), executor);

        handle = std::static_pointer_cast<one::helpers::DirectIOFileHandle>(
            proxy->open(testFileId, O_RDWR, {}).getVia(executor.get()));
    }

    ~DirectIOHelperTest()
    {
        boost::system::error_code ec;
        boost::filesystem::remove_all(root, ec);
    }

    void unlinkOnDIO(boost::filesystem::path p)
    {
        std::remove((root / p).c_str());
    }

    void SetUp() override {}

    void TearDown() override { unlinkOnDIO(testFileId); }

    std::shared_ptr<folly::ManualExecutor> executor;

    boost::filesystem::path root = TEST_ROOT / boost::filesystem::unique_path();

    std::string testFileId = "test.txt"s;
    boost::filesystem::path testFilePath = root / testFileId;

    std::shared_ptr<DirectIOHelper> proxy;
    std::shared_ptr<one::helpers::DirectIOFileHandle> handle;
};

TEST_F(DirectIOHelperTest, shouldWriteBytes)
{
    std::string stmp("000");
    std::string tmp;

    folly::IOBufQueue writeBuf{folly::IOBufQueue::cacheChainLength()};
    writeBuf.append(stmp);

    auto bytes_written =
        handle->write(5, std::move(writeBuf)).getVia(executor.get());

    EXPECT_EQ(3, bytes_written);

    std::ifstream f(testFilePath.string());
    f >> tmp;
    f.close();

    EXPECT_EQ("test_000456789_test", tmp);
}

TEST_F(DirectIOHelperTest, shouldReadBytes)
{
    auto readBuf = handle->read(5, 10).getVia(executor.get());

    std::string data;
    readBuf.appendToString(data);

    EXPECT_EQ(10, data.size());
    EXPECT_EQ("123456789_", data);
}

TEST_F(DirectIOHelperTest, shouldRunSync)
{
    EXPECT_NO_THROW(handle->fsync(false).getVia(executor.get()));
}

TEST_F(DirectIOHelperTest, shouldGetAttributes)
{
    auto stbuf = proxy->getattr(testFileId).getVia(executor.get());
    EXPECT_EQ(20, stbuf.st_size);
}

TEST_F(DirectIOHelperTest, shouldCheckAccess)
{
    EXPECT_NO_THROW(proxy->access(testFileId, 0).getVia(executor.get()));
}

TEST_F(DirectIOHelperTest, shouldNotReadDirectory)
{
    EXPECT_THROW_POSIX_CODE(
        proxy->readdir(testFileId, 0, 10).getVia(executor.get()), ENOSYS);
}

TEST_F(DirectIOHelperTest, mknod)
{
    EXPECT_THROW_POSIX_CODE(
        proxy->mknod(testFileId, S_IFREG, {}, 0).getVia(executor.get()),
        EEXIST);
}

TEST_F(DirectIOHelperTest, shouldMakeDirectory)
{
    EXPECT_NO_THROW(proxy->mkdir("dir", 0).getVia(executor.get()));
    std::remove((root / "dir").c_str());
}

TEST_F(DirectIOHelperTest, shouldDeleteFile)
{
    EXPECT_NO_THROW(proxy->unlink(testFileId).getVia(executor.get()));
}

TEST_F(DirectIOHelperTest, shouldDeleteDir)
{
    EXPECT_THROW_POSIX_CODE(
        proxy->rmdir(testFileId).getVia(executor.get()), ENOTDIR);
}

TEST_F(DirectIOHelperTest, shouldMakeSymlink)
{
    EXPECT_NO_THROW(proxy->symlink("/from", "to").getVia(executor.get()));

    EXPECT_TRUE(boost::filesystem::is_symlink(
        boost::filesystem::symlink_status((root / "to"))));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldReadSymlink)
{
    auto sres = ::symlink((root / "from").c_str(), (root / "to").c_str());
    ASSERT_TRUE(sres == 0);

    EXPECT_EQ(
        (root / "from").string(), proxy->readlink("to").getVia(executor.get()));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldRename)
{
    EXPECT_NO_THROW(proxy->rename(testFileId, "to").getVia(executor.get()));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldCreateLink)
{
    EXPECT_NO_THROW(proxy->link(testFileId, "to").getVia(executor.get()));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldChangeMode)
{
    EXPECT_NO_THROW(proxy->chmod(testFileId, 600).getVia(executor.get()));
}

TEST_F(DirectIOHelperTest, shouldChangeOwner)
{
    EXPECT_NO_THROW(proxy->chown(testFileId, -1, -1).getVia(executor.get()));
}

TEST_F(DirectIOHelperTest, shouldTruncate)
{
    EXPECT_NO_THROW(proxy->truncate(testFileId, 0).getVia(executor.get()));
}
