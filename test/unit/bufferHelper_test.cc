#include "asioExecutor.h"
#include "buffering/bufferAgent.h"
#include "helpers/storageHelper.h"
#include "scheduler.h"

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <folly/FBString.h>
#include <folly/futures/Future.h>
#include <folly/io/IOBufQueue.h>
#include <gtest/gtest.h>

#include <algorithm>
#include <atomic>
#include <chrono>

class FileHandle : public one::helpers::FileHandle {
public:
    FileHandle(const folly::fbstring &fileId, folly::Executor &executor,
        std::atomic<bool> &wasSimultaneous)
        : one::helpers::FileHandle{fileId}
        , m_executor{executor}
        , m_wasSimultaneous{wasSimultaneous}
    {
    }

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override
    {
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    }

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override
    {
        return folly::via(&m_executor, [ this, size = buf.chainLength() ] {
            if (++m_simultaneous > 1)
                m_wasSimultaneous = true;
            std::this_thread::sleep_for(std::chrono::milliseconds(5));
            --m_simultaneous;
            return size;
        });
    }

    const one::helpers::Timeout &timeout() override
    {
        return one::helpers::ASYNC_OPS_TIMEOUT;
    }

private:
    folly::Executor &m_executor;
    std::atomic<bool> &m_wasSimultaneous;
    std::atomic<size_t> m_simultaneous{0};
};

class StorageHelper : public one::helpers::StorageHelper {
public:
    StorageHelper(folly::Executor &executor, std::atomic<bool> &wasSimultaneous)
        : m_executor{executor}
        , m_wasSimultaneous{wasSimultaneous}
    {
    }

    folly::Future<one::helpers::FileHandlePtr> open(
        const folly::fbstring &fileId, const int /*flags*/,
        const one::helpers::Params & /*openParams*/) override
    {
        return folly::makeFuture(static_cast<one::helpers::FileHandlePtr>(
            std::make_shared<FileHandle>(
                fileId, m_executor, m_wasSimultaneous)));
    }

    const one::helpers::Timeout &timeout() override
    {
        return one::helpers::ASYNC_OPS_TIMEOUT;
    }

private:
    folly::Executor &m_executor;
    std::atomic<bool> &m_wasSimultaneous;
};

struct BufferHelperTest : public ::testing::Test {
protected:
    BufferHelperTest()
    {
        for (int i = 0; i < 100; ++i)
            workers.emplace_back(std::thread{[&] { service.run(); }});

        one::helpers::buffering::BufferLimits limits;
        auto wrappedHelper =
            std::make_shared<StorageHelper>(executor, wasSimultaneous);
        helper = std::make_shared<one::helpers::buffering::BufferAgent>(
            limits, wrappedHelper, scheduler);
    }

    ~BufferHelperTest()
    {
        service.stop();
        for (auto &worker : workers)
            worker.join();
    }

    std::atomic<bool> wasSimultaneous{false};
    one::helpers::StorageHelperPtr helper;

private:
    asio::io_service service{100};
    asio::executor_work<asio::io_service::executor_type> idleWork{
        asio::make_work(service)};
    folly::fbvector<std::thread> workers;
    one::Scheduler scheduler{1};
    one::AsioExecutor executor{service};
};

TEST_F(BufferHelperTest, shouldNotWriteSimultaneously)
{
    auto handle = helper->open("fileId", 0, {}).get();
    for (int i = 0; i < 10; ++i) {
        for (int j = 0; j < 1000; ++j) {
            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            buf.allocate(1024 * 1024);
            handle->write(j * 1024 * 1024, std::move(buf));
        }
    }
    handle->release().get();
    ASSERT_FALSE(wasSimultaneous.load());
}
