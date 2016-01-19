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

class NullHelper : public one::helpers::IStorageHelper {
public:
    std::error_code ec;

    void ash_getattr(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::GeneralCallback<struct stat> callback) override
    {
        struct stat st = {};
        callback(st, ec);
    }

    void ash_access(one::helpers::CTXRef, const boost::filesystem::path &, int,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_readlink(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::GeneralCallback<std::string> callback) override
    {
        callback("", ec);
    }

    void ash_readdir(one::helpers::CTXRef, const boost::filesystem::path &,
        off_t, size_t,
        one::helpers::GeneralCallback<const std::vector<std::string> &>
            callback) override
    {
        std::vector<std::string> v;
        callback(v, ec);
    }

    void ash_mknod(one::helpers::CTXRef, const boost::filesystem::path &,
        mode_t, dev_t, one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_mkdir(one::helpers::CTXRef, const boost::filesystem::path &,
        mode_t, one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_unlink(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_rmdir(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_symlink(one::helpers::CTXRef, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_rename(one::helpers::CTXRef, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_link(one::helpers::CTXRef, const boost::filesystem::path &,
        const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_chmod(one::helpers::CTXRef, const boost::filesystem::path &,
        mode_t, one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_chown(one::helpers::CTXRef, const boost::filesystem::path &, uid_t,
        gid_t, one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_truncate(one::helpers::CTXRef, const boost::filesystem::path &,
        off_t, one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_open(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::GeneralCallback<int> callback) override
    {
        callback(0, ec);
    }

    void ash_read(one::helpers::CTXRef, const boost::filesystem::path &,
        asio::mutable_buffer buf, off_t, const std::string &fileUuid,
        one::helpers::GeneralCallback<asio::mutable_buffer> callback) override
    {
        callback(buf, ec);
    }

    void ash_write(one::helpers::CTXRef, const boost::filesystem::path &,
        asio::const_buffer buf, off_t, const std::string &fileUuid,
        one::helpers::GeneralCallback<std::size_t> callback) override
    {
        callback(asio::buffer_size(buf), ec);
    }

    void ash_release(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_flush(one::helpers::CTXRef, const boost::filesystem::path &,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    void ash_fsync(one::helpers::CTXRef, const boost::filesystem::path &, bool,
        one::helpers::VoidCallback callback) override
    {
        callback(ec);
    }

    asio::mutable_buffer sh_read(one::helpers::CTXRef,
        const boost::filesystem::path &, asio::mutable_buffer buf,
        off_t, const std::string &) override
    {
        if (ec)
            throw std::system_error{ec};

        return buf;
    }

    std::size_t sh_write(one::helpers::CTXRef, const boost::filesystem::path &,
        asio::const_buffer buf, off_t, const std::string &) override
    {
        if (ec)
            throw std::system_error{ec};

        return asio::buffer_size(buf);
    }
};

#endif // HELPERS_NULL_HELPER_H
