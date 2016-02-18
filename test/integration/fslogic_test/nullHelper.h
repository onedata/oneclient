/**
 * @file nullHelper.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_NULL_HELPER_H
#define HELPERS_NULL_HELPER_H

#include "helpers/IStorageHelper.h"

#include <gmock/gmock.h>

using ::testing::_;
using ::testing::Invoke;
using ::testing::Mock;

namespace boost {
namespace filesystem {
void PrintTo(const boost::filesystem::path &path, std::ostream *os)
{
    *os << path;
}
}
}

class NullHelperCTX : public one::helpers::IStorageHelperCTX {
    void setFlags(int flags) {}
};

class NullHelper : public one::helpers::IStorageHelper {
public:
    std::error_code m_ec;

    one::helpers::CTXPtr createCTX()
    {
        return std::make_shared<NullHelperCTX>();
    }

    void ash_getattr(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::GeneralCallback<struct stat> callback) override
    {
        struct stat st = {};
        callback(st, m_ec);
    }

    void ash_access(one::helpers::CTXPtr, const boost::filesystem::path &, int,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_readlink(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::GeneralCallback<std::string> callback) override
    {
        callback("", m_ec);
    }

    void ash_readdir(one::helpers::CTXPtr, const boost::filesystem::path &,
        off_t, size_t,
        one::helpers::GeneralCallback<const std::vector<std::string> &>
            callback) override
    {
        std::vector<std::string> v;
        callback(v, m_ec);
    }

    void ash_mknod(one::helpers::CTXPtr, const boost::filesystem::path &,
        mode_t, one::helpers::FlagsSet flags, dev_t,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_mkdir(one::helpers::CTXPtr, const boost::filesystem::path &,
        mode_t, one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_unlink(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_rmdir(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_symlink(one::helpers::CTXPtr, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_rename(one::helpers::CTXPtr, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_link(one::helpers::CTXPtr, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_chmod(one::helpers::CTXPtr, const boost::filesystem::path &,
        mode_t, one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_chown(one::helpers::CTXPtr, const boost::filesystem::path &, uid_t,
        gid_t, one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_truncate(one::helpers::CTXPtr, const boost::filesystem::path &,
        off_t, one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_open(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::FlagsSet,
        one::helpers::GeneralCallback<int> callback) override
    {
        callback(0, m_ec);
    }

    void ash_read(one::helpers::CTXPtr, const boost::filesystem::path &,
        asio::mutable_buffer buf, off_t, const std::string &fileUuid,
        one::helpers::GeneralCallback<asio::mutable_buffer> callback) override
    {
        callback(buf, m_ec);
    }

    void ash_write(one::helpers::CTXPtr, const boost::filesystem::path &,
        asio::const_buffer buf, off_t, const std::string &fileUuid,
        one::helpers::GeneralCallback<std::size_t> callback) override
    {
        callback(asio::buffer_size(buf), m_ec);
    }

    void ash_release(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_flush(one::helpers::CTXPtr, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    void ash_fsync(one::helpers::CTXPtr, const boost::filesystem::path &, bool,
        one::helpers::VoidCallback callback) override
    {
        callback(m_ec);
    }

    asio::mutable_buffer sh_read(one::helpers::CTXPtr,
        const boost::filesystem::path &, asio::mutable_buffer buf, off_t,
        const std::string &) override
    {
        if (m_ec)
            throw std::system_error{m_ec};

        return buf;
    }

    std::size_t sh_write(one::helpers::CTXPtr, const boost::filesystem::path &,
        asio::const_buffer buf, off_t, const std::string &) override
    {
        if (m_ec)
            throw std::system_error{m_ec};

        return asio::buffer_size(buf);
    }

    virtual int sh_open(one::helpers::CTXPtr ctx,
        const boost::filesystem::path &p, one::helpers::FlagsSet) override
    {
        if (m_ec)
            throw std::system_error{m_ec};
        return 0;
    }

    virtual std::error_code sh_release(
        one::helpers::CTXPtr ctx, const boost::filesystem::path &p) override
    {
        if (m_ec)
            throw std::system_error{m_ec};
        return std::error_code();
    }
};

class NullHelperMock : public NullHelper {
public:
    MOCK_METHOD3(
        sh_open, int(one::helpers::CTXPtr, const boost::filesystem::path &,
                     one::helpers::FlagsSet));
    MOCK_METHOD2(sh_release,
        std::error_code(one::helpers::CTXPtr, const boost::filesystem::path &));

    NullHelperMock()
    {
        ON_CALL(*this, sh_open(_, _, _))
            .WillByDefault(Invoke(&m_real, &NullHelper::sh_open));
        ON_CALL(*this, sh_release(_, _))
            .WillByDefault(Invoke(&m_real, &NullHelper::sh_release));
    }

    void expect_call_sh_open(std::string filename, int times)
    {
        EXPECT_CALL(*this, sh_open(_, boost::filesystem::path(filename), _))
            .Times(times)
            .WillRepeatedly(Invoke(&m_real, &NullHelper::sh_open));
    }

    void expect_call_sh_release(std::string filename, int times)
    {
        EXPECT_CALL(*this, sh_release(_, boost::filesystem::path(filename)))
            .Times(times)
            .WillRepeatedly(Invoke(&m_real, &NullHelper::sh_release));
    }

    bool verify_and_clear_expectations()
    {
        return Mock::VerifyAndClearExpectations(this);
    }

    void set_ec(std::error_code ec)
    {
        m_ec = ec;
        m_real.m_ec = ec;
    }

private:
    NullHelper m_real;
};

#endif // HELPERS_NULL_HELPER_H
