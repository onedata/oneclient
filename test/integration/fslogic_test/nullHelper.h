/**
 * @file nullHelper.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_NULL_HELPER_H
#define HELPERS_NULL_HELPER_H

#include "helpers/storageHelper.h"

#include <gmock/gmock.h>

using ::testing::_;
using ::testing::Invoke;
using ::testing::Mock;
using ::testing::Return;

class NullHelperHandle : public one::helpers::FileHandle {
public:
    NullHelperHandle(std::error_code ec)
        : one::helpers::FileHandle{{}}
        , m_ec{ec}
    {
    }

    folly::Future<folly::IOBufQueue> read(
        const off_t, const std::size_t size) override
    {
        if (m_ec)
            return folly::makeFuture<folly::IOBufQueue>(
                std::system_error{m_ec});

        folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
        buf.allocate(size);

        return folly::makeFuture(std::move(buf));
    }

    folly::Future<std::size_t> write(
        const off_t, folly::IOBufQueue buf) override
    {
        if (m_ec)
            return folly::makeFuture<std::size_t>(std::system_error{m_ec});

        return buf.chainLength();
    }

    folly::Future<folly::Unit> release() override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> flush() override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> fsync(bool) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    const one::helpers::Timeout &timeout() override { return m_timeout; }

    std::error_code m_ec;
    one::helpers::Timeout m_timeout{60};
};

struct NullHelperHandleMock : public NullHelperHandle {
    NullHelperHandleMock(std::error_code ec)
        : NullHelperHandle{ec}
        , m_real{ec}
    {
        ON_CALL(*this, release())
            .WillByDefault(Invoke(&m_real, &one::helpers::FileHandle::release));
    }

    MOCK_METHOD0(release, folly::Future<folly::Unit>());
    NullHelperHandle m_real;
};

class NullHelper : public one::helpers::StorageHelper {
public:
    folly::Future<struct stat> getattr(const folly::fbstring &) override
    {
        if (m_ec)
            return folly::makeFuture<struct stat>(std::system_error{m_ec});

        struct stat st = {};
        return st;
    }

    folly::Future<folly::Unit> access(
        const folly::fbstring &fileId, const int mask) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::fbstring> readlink(
        const folly::fbstring &fileId) override
    {
        if (m_ec)
            return folly::makeFuture<folly::fbstring>(std::system_error{m_ec});

        return folly::fbstring{};
    }

    folly::Future<folly::fbvector<folly::fbstring>> readdir(
        const folly::fbstring &fileId, const off_t offset,
        const std::size_t count) override
    {
        if (m_ec)
            return folly::makeFuture<folly::fbvector<folly::fbstring>>(
                std::system_error{m_ec});

        return folly::fbvector<folly::fbstring>{};
    }

    folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const one::helpers::FlagsSet &flags,
        const dev_t rdev) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> unlink(const folly::fbstring &fileId) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> rmdir(const folly::fbstring &fileId) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> symlink(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> rename(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> link(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> chown(const folly::fbstring &fileId,
        const uid_t uid, const gid_t gid) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size) override
    {
        if (m_ec)
            return folly::makeFuture<folly::Unit>(std::system_error{m_ec});

        return folly::makeFuture();
    }

    folly::Future<one::helpers::FileHandlePtr> open(
        const folly::fbstring &fileId, const int flags,
        const one::helpers::Params &openParams) override
    {
        if (m_ec)
            return folly::makeFuture<one::helpers::FileHandlePtr>(
                std::system_error{m_ec});

        m_handles.insert(std::make_pair(
            fileId, std::make_shared<NullHelperHandleMock>(m_ec)));

        return folly::makeFuture(m_handles[fileId]);
    }

    const one::helpers::Timeout &timeout() override { return m_timeout; }

    std::error_code m_ec;
    one::helpers::Timeout m_timeout{60};
    std::unordered_map<folly::fbstring, std::shared_ptr<NullHelperHandleMock>>
        m_handles;
};

struct NullHelperMock : public NullHelper {
    NullHelperMock()
    {
        ON_CALL(*this, open(_, _, _))
            .WillByDefault(Invoke(this, &NullHelperMock::openReal));
    }

    folly::Future<one::helpers::FileHandlePtr> openReal(
        const folly::fbstring &id, const int flag,
        const one::helpers::Params &param)
    {
        return m_real.open(id, flag, param);
    }

    void expect_call_sh_open(folly::fbstring filename, int times)
    {
        EXPECT_CALL(*this, open(filename, _, _)).Times(times);
    }

    void expect_call_sh_release(folly::fbstring filename, int times)
    {
        m_real.m_handles.insert(std::make_pair(
            filename, std::make_shared<NullHelperHandleMock>(m_ec)));
        EXPECT_CALL(*m_real.m_handles[filename], release()).Times(times);
    }

    bool verify_and_clear_expectations()
    {
        return Mock::VerifyAndClearExpectations(this) &&
            std::all_of(
                m_real.m_handles.begin(), m_real.m_handles.end(), [](auto ha) {
                    return Mock::VerifyAndClearExpectations(ha.second.get());
                });
    }

    void set_ec(std::error_code ec)
    {
        m_real.m_ec = ec;
        for (auto ha : m_real.m_handles)
            ha.second->m_real.m_ec = ec;
    }

    MOCK_METHOD3(open,
        folly::Future<one::helpers::FileHandlePtr>(
            const folly::fbstring &, const int, const one::helpers::Params &));

    NullHelper m_real;
};

#endif // HELPERS_NULL_HELPER_H
