/**
 * @file clusterProxyHelper_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "directIOHelper.h"
#include "helpers/storageHelperFactory.h"

#include "testUtils.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <gtest/gtest.h>

#include <errno.h>

#include <cstring>
#include <future>
#include <iostream>
#include <fstream>
#include <cstdio>

#define DIO_TEST_ROOT "/tmp"

using namespace ::testing;
using namespace one;
using namespace one::helpers;
using namespace std::placeholders;
using one::helpers::utils::tolower;

template <typename T> bool identityEqual(const T &lhs, const T &rhs)
{
    return &lhs == &rhs;
}

constexpr int BENCH_BLOCK_SIZE = 1024*100;
constexpr int BENCH_LOOP_COUNT = 10000;

class DirectIOHelperTest : public ::testing::Test {
protected:
    std::shared_ptr<DirectIOHelper> proxy;

    struct fuse_file_info ffi = {0};
    StorageHelperCTX ctx;
    char buf[1024];

    boost::asio::io_service io_service;
    boost::asio::io_service::work io_work;
    std::thread th_handle1, th_handle2, th_handle3, th_handle4;

    boost::filesystem::path testFilePath;
    boost::filesystem::path testFileId;

    DirectIOHelperTest()
        : ctx(ffi)
        , io_work(io_service)
    {
    }

    void unlinkOnDIO(boost::filesystem::path p)
    {
        std::remove((boost::filesystem::path(DIO_TEST_ROOT) / p).c_str());
    }

    void SetUp() override
    {
        testFileId = "test.txt";
        testFilePath = boost::filesystem::path(DIO_TEST_ROOT) / testFileId;

        th_handle1 = std::thread([&]() { io_service.run(); });
        th_handle2 = std::thread([&]() { io_service.run(); });
        th_handle3 = std::thread([&]() { io_service.run(); });
        th_handle4 = std::thread([&]() { io_service.run(); });
        proxy = std::make_shared<DirectIOHelper>(
            IStorageHelper::ArgsMap{{srvArg(0), std::string(DIO_TEST_ROOT)}},
            io_service);

        // remove all files that are used in tests
        unlinkOnDIO("to");
        unlinkOnDIO("dir");
        unlinkOnDIO(testFileId);

        // create test file
        std::ofstream f(testFilePath.string());
        f << "test_123456789_test" << std::endl;
        f.close();
    }

    void TearDown() override
    {
        unlinkOnDIO(testFileId);

        io_service.stop();
        th_handle1.join();
        th_handle2.join();
        th_handle3.join();
        th_handle4.join();
    }
};

TEST_F(DirectIOHelperTest, shouldWriteBytes)
{
    std::string stmp("000");
    std::string tmp;
    auto writeBuf = boost::asio::buffer(stmp);

    auto p2 = proxy->ash_write(testFileId, writeBuf, 5, ctx);
    auto bytes_written = p2.get();
    EXPECT_EQ(3, bytes_written);

    std::ifstream f(testFilePath.string());
    f >> tmp;
    f.close();

    EXPECT_EQ("test_000456789_test", tmp);
}

TEST_F(DirectIOHelperTest, shouldReadBytes)
{
    char stmp[10];
    auto buf1 = boost::asio::mutable_buffer(stmp, 10);

    auto p2 = proxy->ash_read(testFileId, buf1, 5, ctx);
    auto buf2 = p2.get();

    EXPECT_EQ(10, boost::asio::buffer_size(buf2));
    EXPECT_EQ("123456789_", std::string(boost::asio::buffer_cast<const char*>(buf2), boost::asio::buffer_size(buf2)));
}


TEST_F(DirectIOHelperTest, shouldOpen)
{
    auto f1 = proxy->ash_open(testFileId, ctx);
    EXPECT_GT(f1.get(), 0);
    EXPECT_GT(ctx.m_ffi.fh, 0);
}

TEST_F(DirectIOHelperTest, shouldRelease)
{
    auto fd = ::open(testFilePath.c_str(), ctx.m_ffi.flags);
    ctx.m_ffi.fh = fd;

    auto p2 = proxy->ash_release(testFileId, ctx);
    EXPECT_NO_THROW(p2.get());
    EXPECT_EQ(0, ctx.m_ffi.fh);
}

TEST_F(DirectIOHelperTest, shouldRunSync)
{
    auto p = proxy->ash_fsync(testFileId, 0, ctx);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, shouldGetAttributes)
{
    auto p = proxy->ash_getattr(testFileId);
    struct stat stbuf = p.get();
    EXPECT_EQ(20, stbuf.st_size);
}

TEST_F(DirectIOHelperTest, shouldCheckAccess)
{
    auto p = proxy->ash_access(testFileId, 0);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, shouldNotReadDirectory)
{
    auto p = proxy->ash_readdir(testFileId, 0, 10, ctx);
    EXPECT_THROW_POSIX_CODE(p.get(), ENOTSUP);
}

TEST_F(DirectIOHelperTest, mknod)
{
    auto p = proxy->ash_mknod(testFileId, S_IFREG, 0);
    EXPECT_THROW_POSIX_CODE(p.get(), EEXIST);
}

TEST_F(DirectIOHelperTest, shouldMakeDirectory)
{
    auto p = proxy->ash_mkdir("dir", 0);
    EXPECT_NO_THROW(p.get());

    std::remove("dir");
}

TEST_F(DirectIOHelperTest, shouldDeleteFile)
{
    auto p = proxy->ash_unlink(testFileId);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, shouldDeleteDir)
{
    auto p = proxy->ash_rmdir(testFileId);
    EXPECT_THROW_POSIX_CODE(p.get(), ENOTDIR);
}

TEST_F(DirectIOHelperTest, shouldMakeSymlink)
{
    auto p1 = proxy->ash_symlink("/from", "to");
    EXPECT_NO_THROW(p1.get());

    EXPECT_TRUE(boost::filesystem::is_symlink(boost::filesystem::symlink_status((boost::filesystem::path(DIO_TEST_ROOT) / "to"))));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldReadSymlink)
{
    ::symlink((boost::filesystem::path(DIO_TEST_ROOT) / "from").c_str(), (boost::filesystem::path(DIO_TEST_ROOT) / "to").c_str());

    auto p2 = proxy->ash_readlink("to");
    EXPECT_EQ((boost::filesystem::path(DIO_TEST_ROOT) / "from").string(), p2.get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldRename)
{
    auto p = proxy->ash_rename(testFileId, "to");
    EXPECT_NO_THROW(p.get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldCreateLink)
{
    auto p = proxy->ash_link(testFileId, "to");
    EXPECT_NO_THROW(p.get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldChangeMode)
{
    auto p = proxy->ash_chmod(testFileId, 600);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, shouldChangeOwner)
{
    auto p = proxy->ash_chown(testFileId, -1, -1);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, shouldTruncate)
{
    auto p = proxy->ash_truncate(testFileId, 0);
    EXPECT_NO_THROW(p.get());
}

TEST_F(DirectIOHelperTest, AsyncBench)
{
    ctx.m_ffi.flags |= O_RDWR;
    auto f1 = proxy->ash_open(testFileId, ctx);
    f1.get();

    char stmp[BENCH_BLOCK_SIZE];
    future_t<int> res[BENCH_LOOP_COUNT];
    auto writeBuf = boost::asio::buffer(stmp, BENCH_BLOCK_SIZE);

    for(auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
        res[i] = proxy->ash_write(testFileId, writeBuf, 0, ctx);
    }

    for(auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
        res[i].get();
    }

    proxy->ash_release(testFileId, ctx);
}

TEST_F(DirectIOHelperTest, SyncBench)
{
    ctx.m_ffi.flags |= O_RDWR;
    auto f1 = proxy->ash_open(testFileId, ctx);
    f1.get();

    char stmp[BENCH_BLOCK_SIZE];
    auto writeBuf = boost::asio::buffer(stmp, BENCH_BLOCK_SIZE);
    for(auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
        proxy->sh_write(testFileId, writeBuf, 0, ctx);
    }

    proxy->ash_release(testFileId, ctx);
}
