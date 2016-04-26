/**
 * @file directIOHelper_test.cc
 * @author Rafal Slota
 * @copyright (C) 2015 ACK CYFRONET AGH
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

#include <cstdio>
#include <cstring>
#include <fstream>
#include <functional>
#include <future>
#include <iostream>

#define DIO_TEST_ROOT "/tmp"

using namespace ::testing;
using namespace one;
using namespace one::helpers;
using namespace one::testing;
using namespace std::placeholders;

template <typename T> bool identityEqual(const T &lhs, const T &rhs)
{
    return &lhs == &rhs;
}

constexpr int BENCH_BLOCK_SIZE = 1024 * 100;
constexpr int BENCH_LOOP_COUNT = 10000;

class DirectIOHelperTest : public ::testing::Test {
protected:
    std::shared_ptr<DirectIOHelper> proxy;

    std::shared_ptr<PosixHelperCTX> ctx;
    char buf[1024];

    asio::io_service io_service;
    asio::io_service::work io_work;
    std::thread th_handle1, th_handle2, th_handle3, th_handle4;

    boost::filesystem::path testFilePath;
    boost::filesystem::path testFileId;
    std::unordered_map<std::string, std::string> testParameters;

    std::shared_ptr<std::promise<void>> pv1;
    std::shared_ptr<std::promise<void>> pv2;
    std::shared_ptr<std::promise<void>> pv3;

    std::shared_ptr<std::promise<int>> pi1;
    std::shared_ptr<std::promise<int>> pi2;
    std::shared_ptr<std::promise<int>> pi3;

    DirectIOHelperTest()
        : io_work(io_service)
    {
    }

    void unlinkOnDIO(boost::filesystem::path p)
    {
        std::remove((boost::filesystem::path(DIO_TEST_ROOT) / p).c_str());
    }

    void SetUp() override
    {
        pv1 = std::make_shared<std::promise<void>>();
        pv2 = std::make_shared<std::promise<void>>();
        pv3 = std::make_shared<std::promise<void>>();

        pi1 = std::make_shared<std::promise<int>>();
        pi2 = std::make_shared<std::promise<int>>();
        pi3 = std::make_shared<std::promise<int>>();

        testFileId = "test.txt";
        testFilePath = boost::filesystem::path(DIO_TEST_ROOT) / testFileId;
        testParameters = {{"file_uuid", "test_uuid"}};

        th_handle1 = std::thread([&]() { io_service.run(); });
        th_handle2 = std::thread([&]() { io_service.run(); });
        th_handle3 = std::thread([&]() { io_service.run(); });
        th_handle4 = std::thread([&]() { io_service.run(); });
        proxy = std::make_shared<DirectIOHelper>(
            std::unordered_map<std::string, std::string>{
                {"root_path", std::string(DIO_TEST_ROOT)}},
            io_service, DirectIOHelper::linuxUserCTXFactory);

        auto rawCTX = proxy->createCTX(testParameters);
        ctx = std::dynamic_pointer_cast<PosixHelperCTX>(rawCTX);
        ctx->setUserCTX({{"uid", std::to_string(getuid())},
            {"gid", std::to_string(getgid())}});

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

public:
    void set_void_promise(
        std::shared_ptr<std::promise<void>> p, one::helpers::error_t e)
    {
        if (e) {
            p->set_exception(std::make_exception_ptr(std::system_error(e)));
        }
        else {
            p->set_value();
        }
    }

    template <class T>
    void set_promise(
        std::shared_ptr<std::promise<T>> p, T value, one::helpers::error_t e)
    {
        if (e) {
            std::cout << "Error: " << e.message() << std::endl;
            p->set_exception(std::make_exception_ptr(std::system_error(e)));
        }
        else {
            p->set_value(value);
        }
    }

    template <class T> std::shared_ptr<std::promise<T>> make_promise()
    {
        return std::make_shared<std::promise<T>>();
    }
};

class InvalidUserCTX : public DirectIOHelper::UserCTX {
public:
    bool valid() { return false; }
};

TEST_F(DirectIOHelperTest, shouldFaileWithInvalidUserCTX)
{
    DirectIOHelper helper({{"root_path", DIO_TEST_ROOT}}, io_service,
        [](std::shared_ptr<PosixHelperCTX>) {
            return std::make_unique<InvalidUserCTX>();
        });

    helper.ash_open(ctx, testFileId, O_RDONLY,
        std::bind(&DirectIOHelperTest::set_promise<int>, this, pi1, _1, _2));

    EXPECT_THROW_POSIX_CODE(pi1->get_future().get(), EDOM);
}

TEST_F(DirectIOHelperTest, shouldWriteBytes)
{
    std::string stmp("000");
    std::string tmp;
    auto writeBuf = asio::buffer(stmp);

    auto p = make_promise<int>();
    proxy->ash_write(ctx, testFileId, writeBuf, 5,
        std::bind(&DirectIOHelperTest::set_promise<int>, this, p, _1, _2));
    auto bytes_written = p->get_future().get();
    EXPECT_EQ(3, bytes_written);

    std::ifstream f(testFilePath.string());
    f >> tmp;
    f.close();

    EXPECT_EQ("test_000456789_test", tmp);
}

TEST_F(DirectIOHelperTest, shouldReadBytes)
{
    char stmp[10];
    auto buf1 = asio::mutable_buffer(stmp, 10);

    auto p = make_promise<asio::mutable_buffer>();
    proxy->ash_read(ctx, testFileId, buf1, 5,
        std::bind(&DirectIOHelperTest::set_promise<asio::mutable_buffer>, this,
                        p, _1, _2));
    auto buf2 = p->get_future().get();

    EXPECT_EQ(10, asio::buffer_size(buf2));
    EXPECT_EQ("123456789_", std::string(asio::buffer_cast<const char *>(buf2),
                                asio::buffer_size(buf2)));
}

TEST_F(DirectIOHelperTest, shouldOpen)
{
    proxy->ash_open(ctx, testFileId, O_RDONLY,
        std::bind(&DirectIOHelperTest::set_promise<int>, this, pi1, _1, _2));
    EXPECT_GT(pi1->get_future().get(), 0);
    EXPECT_GT(ctx->fh, 0);
}

TEST_F(DirectIOHelperTest, shouldRelease)
{
    auto fd = ::open(testFilePath.c_str(), O_RDONLY);
    ctx->fh = fd;

    proxy->ash_release(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
    EXPECT_EQ(-1, ctx->fh);
}

TEST_F(DirectIOHelperTest, shouldRunSync)
{
    proxy->ash_fsync(ctx, testFileId, 0,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, shouldGetAttributes)
{
    auto p = make_promise<struct stat>();
    proxy->ash_getattr(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_promise<struct stat>, this, p, _1,
                           _2));
    struct stat stbuf = p->get_future().get();
    EXPECT_EQ(20, stbuf.st_size);
}

TEST_F(DirectIOHelperTest, shouldCheckAccess)
{
    proxy->ash_access(ctx, testFileId, 0,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, shouldNotReadDirectory)
{
    auto p = make_promise<std::vector<std::string>>();
    proxy->ash_readdir(ctx, testFileId, 0, 10,
        std::bind(&DirectIOHelperTest::set_promise<std::vector<std::string>>,
                           this, p, _1, _2));
    EXPECT_THROW_POSIX_CODE(p->get_future().get(), ENOTSUP);
}

TEST_F(DirectIOHelperTest, mknod)
{
    proxy->ash_mknod(ctx, testFileId, S_IFREG, {}, 0,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_THROW_POSIX_CODE(pv1->get_future().get(), EEXIST);
}

TEST_F(DirectIOHelperTest, shouldMakeDirectory)
{
    proxy->ash_mkdir(ctx, "dir", 0,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());

    std::remove("dir");
}

TEST_F(DirectIOHelperTest, shouldDeleteFile)
{
    proxy->ash_unlink(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, shouldDeleteDir)
{
    proxy->ash_rmdir(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_THROW_POSIX_CODE(pv1->get_future().get(), ENOTDIR);
}

TEST_F(DirectIOHelperTest, shouldMakeSymlink)
{
    proxy->ash_symlink(ctx, "/from", "to",
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());

    EXPECT_TRUE(boost::filesystem::is_symlink(boost::filesystem::symlink_status(
        (boost::filesystem::path(DIO_TEST_ROOT) / "to"))));

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldReadSymlink)
{
    auto sres =
        ::symlink((boost::filesystem::path(DIO_TEST_ROOT) / "from").c_str(),
            (boost::filesystem::path(DIO_TEST_ROOT) / "to").c_str());
    ASSERT_TRUE(sres == 0);

    auto p = make_promise<std::string>();
    proxy->ash_readlink(
        ctx, "to", std::bind(&DirectIOHelperTest::set_promise<std::string>,
                       this, p, _1, _2));
    EXPECT_EQ((boost::filesystem::path(DIO_TEST_ROOT) / "from").string(),
        p->get_future().get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldRename)
{
    proxy->ash_rename(ctx, testFileId, "to",
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldCreateLink)
{
    proxy->ash_link(ctx, testFileId, "to",
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());

    unlinkOnDIO("to");
}

TEST_F(DirectIOHelperTest, shouldChangeMode)
{
    proxy->ash_chmod(ctx, testFileId, 600,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, shouldChangeOwner)
{
    proxy->ash_chown(ctx, testFileId, -1, -1,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, shouldTruncate)
{
    proxy->ash_truncate(ctx, testFileId, 0,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    EXPECT_NO_THROW(pv1->get_future().get());
}

TEST_F(DirectIOHelperTest, AsyncBench)
{
    proxy->ash_open(ctx, testFileId, O_RDWR,
        std::bind(&DirectIOHelperTest::set_promise<int>, this, pi1, _1, _2));
    pi1->get_future().get();

    char stmp[BENCH_BLOCK_SIZE];
    future_t<int> res[BENCH_LOOP_COUNT];
    auto writeBuf = asio::buffer(stmp, BENCH_BLOCK_SIZE);

    for (auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
        auto p = make_promise<int>();
        proxy->ash_write(ctx, testFileId, writeBuf, 0,
            std::bind(&DirectIOHelperTest::set_promise<int>, this, p, _1, _2));
        res[i] = p->get_future();
    }

    for (auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
        res[i].get();
    }

    proxy->ash_release(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv1, _1));
    pv1->get_future().get();
}

TEST_F(DirectIOHelperTest, SyncBench)
{
    proxy->ash_open(ctx, testFileId, O_RDWR, [=](int, one::helpers::error_t e) {
        char stmp[BENCH_BLOCK_SIZE];
        auto writeBuf = asio::buffer(stmp, BENCH_BLOCK_SIZE);
        for (auto i = 0; i < BENCH_LOOP_COUNT; ++i) {
            proxy->sh_write(ctx, testFileId, writeBuf, 0);
        }
        pv1->set_value();
    });
    pv1->get_future().get();

    proxy->ash_release(ctx, testFileId,
        std::bind(&DirectIOHelperTest::set_void_promise, this, pv2, _1));
    pv2->get_future().get();
}
